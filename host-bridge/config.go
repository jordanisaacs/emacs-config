package bridge

import (
	"crypto/rand"
	"encoding/hex"
	"errors"
	"fmt"
	"mime"
	"net"
	"net/url"
	"os"
	"path/filepath"
	"strings"
)

const (
	defaultListenAddress   = "127.0.0.1:24545"
	defaultBaseURL         = "http://[::1]:24545"
	maxTextClipboardBytes  = 4 << 20
	maxImageClipboardBytes = 25 << 20
	maxClipboardBytes      = maxImageClipboardBytes
	maxJSONBytes           = 64 << 10
	defaultTextMIME        = "text/plain;charset=utf-8"
)

var clipboardImageTypes = []string{
	"image/png",
	"image/jpeg",
	"image/tiff",
	"image/gif",
	"image/bmp",
	"image/webp",
}

func normalizeClipboardMIME(raw string) (string, error) {
	if strings.TrimSpace(raw) == "" {
		return defaultTextMIME, nil
	}
	mediaType, parameters, err := mime.ParseMediaType(raw)
	if err != nil {
		return "", errors.New("invalid clipboard MIME type")
	}
	mediaType = strings.ToLower(mediaType)
	if mediaType == "text/plain" {
		charset := strings.ToLower(parameters["charset"])
		if charset != "" && charset != "utf-8" && charset != "utf8" {
			return "", errors.New("clipboard text must use UTF-8")
		}
		return defaultTextMIME, nil
	}
	for _, imageType := range clipboardImageTypes {
		if mediaType == imageType && len(parameters) == 0 {
			return mediaType, nil
		}
	}
	return "", errors.New("unsupported clipboard MIME type")
}

func clipboardLimit(mimeType string) int64 {
	if mimeType == defaultTextMIME {
		return maxTextClipboardBytes
	}
	return maxImageClipboardBytes
}

func defaultTokenFile() (string, error) {
	if path := os.Getenv("EMACS_HOST_BRIDGE_TOKEN_FILE"); path != "" {
		return path, nil
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("find home directory: %w", err)
	}
	return filepath.Join(home, ".config", "emacs-host-bridge", "token"), nil
}

func readToken(path string) (string, error) {
	contents, err := os.ReadFile(path)
	if err != nil {
		return "", fmt.Errorf("read token file %s: %w", path, err)
	}
	token := strings.TrimSpace(string(contents))
	if len(token) < 32 {
		return "", errors.New("bridge token must contain at least 32 characters")
	}
	return token, nil
}

func initializeToken(path string) error {
	if _, err := os.Stat(path); err == nil {
		return fmt.Errorf("token file already exists: %s", path)
	} else if !errors.Is(err, os.ErrNotExist) {
		return fmt.Errorf("inspect token file %s: %w", path, err)
	}

	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		return fmt.Errorf("create token directory: %w", err)
	}
	bytes := make([]byte, 32)
	if _, err := rand.Read(bytes); err != nil {
		return fmt.Errorf("generate token: %w", err)
	}
	file, err := os.OpenFile(path, os.O_WRONLY|os.O_CREATE|os.O_EXCL, 0o600)
	if err != nil {
		return fmt.Errorf("create token file %s: %w", path, err)
	}
	defer file.Close()
	if _, err := fmt.Fprintln(file, hex.EncodeToString(bytes)); err != nil {
		return fmt.Errorf("write token file %s: %w", path, err)
	}
	return nil
}

func baseURLFromEnvironment() (*url.URL, error) {
	raw := os.Getenv("EMACS_HOST_BRIDGE_URL")
	if raw == "" {
		raw = defaultBaseURL
	}
	parsed, err := url.Parse(raw)
	if err != nil {
		return nil, fmt.Errorf("parse EMACS_HOST_BRIDGE_URL: %w", err)
	}
	if parsed.Scheme != "http" {
		return nil, errors.New("EMACS_HOST_BRIDGE_URL must use http over a loopback SSH forward")
	}
	if parsed.Path != "" && parsed.Path != "/" {
		return nil, errors.New("EMACS_HOST_BRIDGE_URL must not contain a path")
	}
	if !loopbackHost(parsed.Hostname()) {
		return nil, errors.New("EMACS_HOST_BRIDGE_URL must point to a loopback address")
	}
	return parsed, nil
}

func validateListenAddress(address string) error {
	host, _, err := net.SplitHostPort(address)
	if err != nil {
		return fmt.Errorf("parse listen address: %w", err)
	}
	if !loopbackHost(host) {
		return errors.New("host bridge must listen on a loopback address")
	}
	return nil
}

func loopbackHost(host string) bool {
	if strings.EqualFold(host, "localhost") {
		return true
	}
	ip := net.ParseIP(host)
	return ip != nil && ip.IsLoopback()
}
