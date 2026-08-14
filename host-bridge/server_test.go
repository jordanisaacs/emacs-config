package bridge

import (
	"bytes"
	"context"
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

type fakeHost struct {
	notification   notificationRequest
	openedURL      string
	writtenMIME    string
	writtenText    []byte
	clipboard      []byte
	clipboardMIME  string
	clipboardTypes []string
	err            error
}

func (host *fakeHost) Notify(_ context.Context, notification notificationRequest) error {
	host.notification = notification
	return host.err
}

func (host *fakeHost) OpenURL(_ context.Context, target string) error {
	host.openedURL = target
	return host.err
}

func (host *fakeHost) WriteClipboard(_ context.Context, mimeType string, text []byte) error {
	host.writtenMIME = mimeType
	host.writtenText = append([]byte(nil), text...)
	return host.err
}

func (host *fakeHost) ReadClipboard(_ context.Context, mimeType string) ([]byte, error) {
	host.clipboardMIME = mimeType
	return append([]byte(nil), host.clipboard...), host.err
}

func (host *fakeHost) ClipboardTypes(_ context.Context) ([]string, error) {
	return append([]string(nil), host.clipboardTypes...), host.err
}

func TestBridgeHandlerRejectsMissingToken(t *testing.T) {
	handler := &bridgeHandler{token: strings.Repeat("a", 32), host: &fakeHost{}}
	request := httptest.NewRequest(http.MethodGet, "/v1/health", nil)
	response := httptest.NewRecorder()
	handler.ServeHTTP(response, request)
	if response.Code != http.StatusUnauthorized {
		t.Fatalf("status = %d, want %d", response.Code, http.StatusUnauthorized)
	}
}

func TestBridgeHandlerDispatchesNotification(t *testing.T) {
	host := &fakeHost{}
	token := strings.Repeat("a", 32)
	handler := &bridgeHandler{token: token, host: host}
	payload, err := json.Marshal(notificationRequest{
		Title:   "Build finished",
		Body:    "Everything passed",
		AppName: "Emacs",
		Urgency: "normal",
	})
	if err != nil {
		t.Fatal(err)
	}
	request := authenticatedRequest(http.MethodPost, "/v1/notify", token, bytes.NewReader(payload))
	response := httptest.NewRecorder()
	handler.ServeHTTP(response, request)
	if response.Code != http.StatusNoContent {
		t.Fatalf("status = %d, want %d: %s", response.Code, http.StatusNoContent, response.Body.String())
	}
	if host.notification.Title != "Build finished" || host.notification.AppName != "Emacs" {
		t.Fatalf("notification = %#v", host.notification)
	}
}

func TestBridgeHandlerAllowsOnlyHTTPURLs(t *testing.T) {
	host := &fakeHost{}
	token := strings.Repeat("a", 32)
	handler := &bridgeHandler{token: token, host: host}

	for _, target := range []string{"file:///etc/passwd", "javascript:alert(1)", "ssh://host"} {
		payload, err := json.Marshal(openRequest{URL: target})
		if err != nil {
			t.Fatal(err)
		}
		request := authenticatedRequest(http.MethodPost, "/v1/open", token, bytes.NewReader(payload))
		response := httptest.NewRecorder()
		handler.ServeHTTP(response, request)
		if response.Code != http.StatusBadRequest {
			t.Errorf("target %q: status = %d, want %d", target, response.Code, http.StatusBadRequest)
		}
	}

	payload, err := json.Marshal(openRequest{URL: "https://example.com/path?q=1"})
	if err != nil {
		t.Fatal(err)
	}
	request := authenticatedRequest(http.MethodPost, "/v1/open", token, bytes.NewReader(payload))
	response := httptest.NewRecorder()
	handler.ServeHTTP(response, request)
	if response.Code != http.StatusNoContent {
		t.Fatalf("status = %d, want %d", response.Code, http.StatusNoContent)
	}
	if host.openedURL != "https://example.com/path?q=1" {
		t.Fatalf("opened URL = %q", host.openedURL)
	}
}

func TestBridgeHandlerClipboardReadRequiresOptIn(t *testing.T) {
	token := strings.Repeat("a", 32)
	host := &fakeHost{clipboard: []byte("host text")}
	handler := &bridgeHandler{token: token, host: host}
	request := authenticatedRequest(http.MethodGet, "/v1/clipboard", token, nil)
	response := httptest.NewRecorder()
	handler.ServeHTTP(response, request)
	if response.Code != http.StatusForbidden {
		t.Fatalf("status = %d, want %d", response.Code, http.StatusForbidden)
	}

	handler.allowClipboardRead = true
	request = authenticatedRequest(http.MethodGet, "/v1/clipboard", token, nil)
	response = httptest.NewRecorder()
	handler.ServeHTTP(response, request)
	if response.Code != http.StatusOK {
		t.Fatalf("status = %d, want %d", response.Code, http.StatusOK)
	}
	if response.Body.String() != "host text" {
		t.Fatalf("clipboard = %q", response.Body.String())
	}
}

func TestBridgeHandlerWritesClipboardExactly(t *testing.T) {
	token := strings.Repeat("a", 32)
	host := &fakeHost{}
	handler := &bridgeHandler{token: token, host: host}
	request := authenticatedRequest(http.MethodPut, "/v1/clipboard", token, strings.NewReader("first\nsecond\n"))
	response := httptest.NewRecorder()
	handler.ServeHTTP(response, request)
	if response.Code != http.StatusNoContent {
		t.Fatalf("status = %d, want %d", response.Code, http.StatusNoContent)
	}
	if string(host.writtenText) != "first\nsecond\n" {
		t.Fatalf("clipboard write = %q", host.writtenText)
	}
	if host.writtenMIME != defaultTextMIME {
		t.Fatalf("clipboard MIME = %q", host.writtenMIME)
	}
}

func TestBridgeHandlerPreservesImageClipboardBytes(t *testing.T) {
	token := strings.Repeat("a", 32)
	host := &fakeHost{clipboard: []byte{0x89, 'P', 'N', 'G'}}
	handler := &bridgeHandler{token: token, allowClipboardRead: true, host: host}

	write := authenticatedRequest(http.MethodPut, "/v1/clipboard", token, bytes.NewReader(host.clipboard))
	write.Header.Set("Content-Type", "image/png")
	response := httptest.NewRecorder()
	handler.ServeHTTP(response, write)
	if response.Code != http.StatusNoContent {
		t.Fatalf("write status = %d, want %d: %s", response.Code, http.StatusNoContent, response.Body.String())
	}
	if host.writtenMIME != "image/png" || !bytes.Equal(host.writtenText, host.clipboard) {
		t.Fatalf("clipboard write = %q %v", host.writtenMIME, host.writtenText)
	}

	read := authenticatedRequest(http.MethodGet, "/v1/clipboard?type=image%2Fpng", token, nil)
	response = httptest.NewRecorder()
	handler.ServeHTTP(response, read)
	if response.Code != http.StatusOK {
		t.Fatalf("read status = %d, want %d: %s", response.Code, http.StatusOK, response.Body.String())
	}
	if host.clipboardMIME != "image/png" || !bytes.Equal(response.Body.Bytes(), host.clipboard) {
		t.Fatalf("clipboard read = %q %v", host.clipboardMIME, response.Body.Bytes())
	}
}

func authenticatedRequest(method, target, token string, body io.Reader) *http.Request {
	var request *http.Request
	if body == nil {
		request = httptest.NewRequest(method, target, nil)
	} else {
		request = httptest.NewRequest(method, target, body)
	}
	request.Header.Set("Authorization", "Bearer "+token)
	return request
}
