package bridge

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strings"
	"time"
)

type bridgeClient struct {
	baseURL *url.URL
	token   string
	http    *http.Client
}

func newBridgeClientFromEnvironment() (*bridgeClient, error) {
	baseURL, err := baseURLFromEnvironment()
	if err != nil {
		return nil, err
	}
	tokenFile, err := defaultTokenFile()
	if err != nil {
		return nil, err
	}
	token, err := readToken(tokenFile)
	if err != nil {
		return nil, err
	}
	return &bridgeClient{
		baseURL: baseURL,
		token:   token,
		http:    &http.Client{Timeout: 20 * time.Second},
	}, nil
}

func (client *bridgeClient) notify(ctx context.Context, request notificationRequest) error {
	return client.sendJSON(ctx, http.MethodPost, "/v1/notify", request)
}

func (client *bridgeClient) open(ctx context.Context, target string) error {
	return client.sendJSON(ctx, http.MethodPost, "/v1/open", openRequest{URL: target})
}

func (client *bridgeClient) writeClipboard(ctx context.Context, mimeType string, contents []byte) error {
	_, err := client.do(ctx, http.MethodPut, "/v1/clipboard", mimeType, contents)
	return err
}

func (client *bridgeClient) readClipboard(ctx context.Context, mimeType string) ([]byte, error) {
	return client.do(
		ctx,
		http.MethodGet,
		"/v1/clipboard?type="+url.QueryEscape(mimeType),
		"",
		nil,
	)
}

func (client *bridgeClient) clipboardTypes(ctx context.Context) ([]string, error) {
	body, err := client.do(ctx, http.MethodGet, "/v1/clipboard/types", "", nil)
	if err != nil {
		return nil, err
	}
	var response struct {
		Types []string `json:"types"`
	}
	if err := json.Unmarshal(body, &response); err != nil {
		return nil, fmt.Errorf("decode clipboard types: %w", err)
	}
	return response.Types, nil
}

func (client *bridgeClient) health(ctx context.Context) error {
	_, err := client.do(ctx, http.MethodGet, "/v1/health", "", nil)
	return err
}

func (client *bridgeClient) sendJSON(ctx context.Context, method, path string, request any) error {
	body, err := json.Marshal(request)
	if err != nil {
		return fmt.Errorf("encode request: %w", err)
	}
	_, err = client.do(ctx, method, path, "application/json", body)
	return err
}

func (client *bridgeClient) do(
	ctx context.Context,
	method string,
	path string,
	contentType string,
	body []byte,
) ([]byte, error) {
	reference, err := url.Parse(path)
	if err != nil {
		return nil, fmt.Errorf("create request URL: %w", err)
	}
	target := client.baseURL.ResolveReference(reference)
	request, err := http.NewRequestWithContext(ctx, method, target.String(), bytes.NewReader(body))
	if err != nil {
		return nil, fmt.Errorf("create request: %w", err)
	}
	request.Header.Set("Authorization", "Bearer "+client.token)
	if contentType != "" {
		request.Header.Set("Content-Type", contentType)
	}
	response, err := client.http.Do(request)
	if err != nil {
		return nil, fmt.Errorf("host bridge unavailable: %w", err)
	}
	defer response.Body.Close()

	limit := int64(maxClipboardBytes + 1)
	if response.StatusCode < 200 || response.StatusCode >= 300 {
		limit = 8 << 10
	}
	responseBody, readErr := io.ReadAll(io.LimitReader(response.Body, limit))
	if readErr != nil {
		return nil, fmt.Errorf("read host bridge response: %w", readErr)
	}
	if response.StatusCode < 200 || response.StatusCode >= 300 {
		message := strings.TrimSpace(string(responseBody))
		if message == "" {
			message = response.Status
		}
		return nil, errors.New(message)
	}
	if len(responseBody) > maxClipboardBytes {
		return nil, errors.New("host clipboard exceeds size limit")
	}
	return responseBody, nil
}
