package bridge

import (
	"context"
	"crypto/subtle"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log"
	"net"
	"net/http"
	"net/url"
	"os"
	"os/signal"
	"strings"
	"syscall"
	"time"
	"unicode/utf8"
)

type notificationRequest struct {
	Title         string `json:"title"`
	Body          string `json:"body,omitempty"`
	AppName       string `json:"app_name,omitempty"`
	Urgency       string `json:"urgency,omitempty"`
	ExpireTime    int    `json:"expire_time_ms,omitempty"`
	FocusBundleID string `json:"focus_bundle_id,omitempty"`
	FocusTTY      string `json:"focus_tty,omitempty"`
}

type openRequest struct {
	URL string `json:"url"`
}

type hostActions interface {
	Notify(context.Context, notificationRequest) error
	OpenURL(context.Context, string) error
	WriteClipboard(context.Context, string, []byte) error
	ReadClipboard(context.Context, string) ([]byte, error)
	ClipboardTypes(context.Context) ([]string, error)
	ClipboardFiles(context.Context) ([]string, error)
}

type bridgeHandler struct {
	token              string
	allowClipboardRead bool
	host               hostActions
}

func (handler *bridgeHandler) ServeHTTP(writer http.ResponseWriter, request *http.Request) {
	if !handler.authorized(request) {
		http.Error(writer, "unauthorized", http.StatusUnauthorized)
		return
	}

	switch request.URL.Path {
	case "/v1/health":
		handler.handleHealth(writer, request)
	case "/v1/notify":
		handler.handleNotify(writer, request)
	case "/v1/open":
		handler.handleOpen(writer, request)
	case "/v1/clipboard":
		handler.handleClipboard(writer, request)
	case "/v1/clipboard/types":
		handler.handleClipboardTypes(writer, request)
	case "/v1/clipboard/files":
		handler.handleClipboardFiles(writer, request)
	default:
		http.Error(writer, "not found", http.StatusNotFound)
	}
}

func (handler *bridgeHandler) authorized(request *http.Request) bool {
	const prefix = "Bearer "
	header := request.Header.Get("Authorization")
	if !strings.HasPrefix(header, prefix) {
		return false
	}
	provided := strings.TrimPrefix(header, prefix)
	return subtle.ConstantTimeCompare([]byte(provided), []byte(handler.token)) == 1
}

func (handler *bridgeHandler) handleHealth(writer http.ResponseWriter, request *http.Request) {
	if request.Method != http.MethodGet {
		methodNotAllowed(writer, http.MethodGet)
		return
	}
	writer.Header().Set("Content-Type", "application/json")
	_, _ = io.WriteString(writer, "{\"ok\":true}\n")
}

func (handler *bridgeHandler) handleNotify(writer http.ResponseWriter, request *http.Request) {
	if request.Method != http.MethodPost {
		methodNotAllowed(writer, http.MethodPost)
		return
	}
	var notification notificationRequest
	if err := decodeJSON(writer, request, &notification); err != nil {
		http.Error(writer, err.Error(), http.StatusBadRequest)
		return
	}
	if notification.Title == "" {
		http.Error(writer, "notification title is required", http.StatusBadRequest)
		return
	}
	if len(notification.Title) > 1024 || len(notification.Body) > 16<<10 || len(notification.AppName) > 1024 || len(notification.FocusBundleID) > 255 || len(notification.FocusTTY) > 128 {
		http.Error(writer, "notification is too large", http.StatusRequestEntityTooLarge)
		return
	}
	if !validBundleIdentifier(notification.FocusBundleID) {
		http.Error(writer, "invalid notification focus bundle identifier", http.StatusBadRequest)
		return
	}
	if !validTerminalTTY(notification.FocusTTY) {
		http.Error(writer, "invalid notification focus tty", http.StatusBadRequest)
		return
	}
	ctx, cancel := context.WithTimeout(request.Context(), 5*time.Second)
	defer cancel()
	if err := handler.host.Notify(ctx, notification); err != nil {
		http.Error(writer, "notification failed", http.StatusBadGateway)
		return
	}
	writer.WriteHeader(http.StatusNoContent)
}

func (handler *bridgeHandler) handleOpen(writer http.ResponseWriter, request *http.Request) {
	if request.Method != http.MethodPost {
		methodNotAllowed(writer, http.MethodPost)
		return
	}
	var open openRequest
	if err := decodeJSON(writer, request, &open); err != nil {
		http.Error(writer, err.Error(), http.StatusBadRequest)
		return
	}
	if err := validateOpenURL(open.URL); err != nil {
		http.Error(writer, err.Error(), http.StatusBadRequest)
		return
	}
	ctx, cancel := context.WithTimeout(request.Context(), 5*time.Second)
	defer cancel()
	if err := handler.host.OpenURL(ctx, open.URL); err != nil {
		http.Error(writer, "open failed", http.StatusBadGateway)
		return
	}
	writer.WriteHeader(http.StatusNoContent)
}

func (handler *bridgeHandler) handleClipboard(writer http.ResponseWriter, request *http.Request) {
	switch request.Method {
	case http.MethodPut:
		handler.handleClipboardWrite(writer, request)
	case http.MethodGet:
		handler.handleClipboardRead(writer, request)
	default:
		writer.Header().Set("Allow", http.MethodGet+", "+http.MethodPut)
		http.Error(writer, "method not allowed", http.StatusMethodNotAllowed)
	}
}

func (handler *bridgeHandler) handleClipboardWrite(writer http.ResponseWriter, request *http.Request) {
	mimeType, err := normalizeClipboardMIME(request.Header.Get("Content-Type"))
	if err != nil {
		http.Error(writer, err.Error(), http.StatusUnsupportedMediaType)
		return
	}
	limit := clipboardLimit(mimeType)
	request.Body = http.MaxBytesReader(writer, request.Body, limit)
	contents, err := io.ReadAll(request.Body)
	if err != nil {
		http.Error(writer, "clipboard data exceeds size limit", http.StatusRequestEntityTooLarge)
		return
	}
	if mimeType == defaultTextMIME && !utf8.Valid(contents) {
		http.Error(writer, "clipboard text must be UTF-8", http.StatusBadRequest)
		return
	}
	ctx, cancel := context.WithTimeout(request.Context(), 15*time.Second)
	defer cancel()
	if err := handler.host.WriteClipboard(ctx, mimeType, contents); err != nil {
		http.Error(writer, "clipboard write failed", http.StatusBadGateway)
		return
	}
	writer.WriteHeader(http.StatusNoContent)
}

func (handler *bridgeHandler) handleClipboardRead(writer http.ResponseWriter, request *http.Request) {
	if !handler.allowClipboardRead {
		http.Error(writer, "clipboard reads are disabled", http.StatusForbidden)
		return
	}
	mimeType, err := normalizeClipboardMIME(request.URL.Query().Get("type"))
	if err != nil {
		http.Error(writer, err.Error(), http.StatusUnsupportedMediaType)
		return
	}
	ctx, cancel := context.WithTimeout(request.Context(), 15*time.Second)
	defer cancel()
	contents, err := handler.host.ReadClipboard(ctx, mimeType)
	if err != nil {
		http.Error(writer, "clipboard read failed", http.StatusBadGateway)
		return
	}
	if int64(len(contents)) > clipboardLimit(mimeType) {
		http.Error(writer, "host clipboard exceeds size limit", http.StatusRequestEntityTooLarge)
		return
	}
	if mimeType == defaultTextMIME && !utf8.Valid(contents) {
		http.Error(writer, "host clipboard is not UTF-8 text", http.StatusUnsupportedMediaType)
		return
	}
	writer.Header().Set("Content-Type", mimeType)
	_, _ = writer.Write(contents)
}

func (handler *bridgeHandler) handleClipboardTypes(writer http.ResponseWriter, request *http.Request) {
	if request.Method != http.MethodGet {
		methodNotAllowed(writer, http.MethodGet)
		return
	}
	if !handler.allowClipboardRead {
		http.Error(writer, "clipboard reads are disabled", http.StatusForbidden)
		return
	}
	ctx, cancel := context.WithTimeout(request.Context(), 15*time.Second)
	defer cancel()
	types, err := handler.host.ClipboardTypes(ctx)
	if err != nil {
		http.Error(writer, "clipboard type query failed", http.StatusBadGateway)
		return
	}
	writer.Header().Set("Content-Type", "application/json")
	if err := json.NewEncoder(writer).Encode(struct {
		Types []string `json:"types"`
	}{Types: types}); err != nil {
		return
	}
}

func (handler *bridgeHandler) handleClipboardFiles(writer http.ResponseWriter, request *http.Request) {
	if request.Method != http.MethodGet {
		methodNotAllowed(writer, http.MethodGet)
		return
	}
	if !handler.allowClipboardRead {
		http.Error(writer, "clipboard reads are disabled", http.StatusForbidden)
		return
	}
	ctx, cancel := context.WithTimeout(request.Context(), maxClipboardFileTransferDuration)
	defer cancel()
	paths, err := handler.host.ClipboardFiles(ctx)
	if err != nil {
		http.Error(writer, "clipboard file query failed", http.StatusBadGateway)
		return
	}
	if len(paths) == 0 {
		http.Error(writer, "clipboard has no files", http.StatusNotFound)
		return
	}
	entries, err := prepareClipboardArchive(ctx, paths)
	if err != nil {
		http.Error(writer, err.Error(), http.StatusUnprocessableEntity)
		return
	}
	writer.Header().Set("Content-Type", clipboardArchiveMIME)
	writer.Header().Set("X-Content-Type-Options", "nosniff")
	if err := writeClipboardArchive(ctx, writer, entries); err != nil {
		log.Printf("clipboard file transfer failed: %v", err)
	}
}

func decodeJSON(writer http.ResponseWriter, request *http.Request, destination any) error {
	request.Body = http.MaxBytesReader(writer, request.Body, maxJSONBytes)
	decoder := json.NewDecoder(request.Body)
	decoder.DisallowUnknownFields()
	if err := decoder.Decode(destination); err != nil {
		return fmt.Errorf("invalid JSON: %w", err)
	}
	if err := decoder.Decode(&struct{}{}); !errors.Is(err, io.EOF) {
		return errors.New("invalid JSON: multiple values")
	}
	return nil
}

func validateOpenURL(target string) error {
	if len(target) > 16<<10 {
		return errors.New("URL is too large")
	}
	parsed, err := url.Parse(target)
	if err != nil {
		return errors.New("invalid URL")
	}
	if parsed.Scheme != "http" && parsed.Scheme != "https" {
		return errors.New("only http and https URLs may be opened")
	}
	if parsed.Host == "" {
		return errors.New("URL must include a host")
	}
	return nil
}

func methodNotAllowed(writer http.ResponseWriter, allowed string) {
	writer.Header().Set("Allow", allowed)
	http.Error(writer, "method not allowed", http.StatusMethodNotAllowed)
}

func serve(address, token string, allowClipboardRead bool, host hostActions) error {
	if err := validateListenAddress(address); err != nil {
		return err
	}
	listener, err := net.Listen("tcp", address)
	if err != nil {
		return fmt.Errorf("listen on %s: %w", address, err)
	}
	defer listener.Close()

	server := &http.Server{
		Handler:           &bridgeHandler{token: token, allowClipboardRead: allowClipboardRead, host: host},
		ReadHeaderTimeout: 2 * time.Second,
		ReadTimeout:       20 * time.Second,
		WriteTimeout:      20 * time.Second,
		IdleTimeout:       30 * time.Second,
		ErrorLog:          log.New(os.Stderr, "host-bridge: ", log.LstdFlags),
	}

	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer stop()
	result := make(chan error, 1)
	go func() {
		result <- server.Serve(listener)
	}()
	log.Printf("emacs-host-bridge listening on %s (clipboard reads: %t)", address, allowClipboardRead)

	select {
	case err := <-result:
		if errors.Is(err, http.ErrServerClosed) {
			return nil
		}
		return err
	case <-ctx.Done():
		shutdownCtx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancel()
		return server.Shutdown(shutdownCtx)
	}
}
