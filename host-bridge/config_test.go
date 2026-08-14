package bridge

import (
	"os"
	"path/filepath"
	"testing"
)

func TestInitializeTokenCreatesPrivateToken(t *testing.T) {
	path := filepath.Join(t.TempDir(), "config", "token")
	if err := initializeToken(path); err != nil {
		t.Fatal(err)
	}
	token, err := readToken(path)
	if err != nil {
		t.Fatal(err)
	}
	if len(token) != 64 {
		t.Fatalf("token length = %d, want 64", len(token))
	}
	info, err := os.Stat(path)
	if err != nil {
		t.Fatal(err)
	}
	if permission := info.Mode().Perm(); permission != 0o600 {
		t.Fatalf("token permission = %o, want 600", permission)
	}
	if err := initializeToken(path); err == nil {
		t.Fatal("expected refusing to overwrite token")
	}
}

func TestValidateListenAddressRejectsNonLoopback(t *testing.T) {
	if err := validateListenAddress("0.0.0.0:24545"); err == nil {
		t.Fatal("expected non-loopback address to be rejected")
	}
	if err := validateListenAddress("127.0.0.1:24545"); err != nil {
		t.Fatal(err)
	}
}

func TestDefaultBaseURLUsesIPv6Loopback(t *testing.T) {
	t.Setenv("EMACS_HOST_BRIDGE_URL", "")
	baseURL, err := baseURLFromEnvironment()
	if err != nil {
		t.Fatal(err)
	}
	if got, want := baseURL.String(), "http://[::1]:24545"; got != want {
		t.Fatalf("default base URL = %q, want %q", got, want)
	}
}

func TestNormalizeClipboardMIME(t *testing.T) {
	for _, value := range []string{"", "text/plain", "text/plain; charset=UTF-8"} {
		mimeType, err := normalizeClipboardMIME(value)
		if err != nil {
			t.Fatalf("normalize %q: %v", value, err)
		}
		if mimeType != defaultTextMIME {
			t.Fatalf("normalize %q = %q", value, mimeType)
		}
	}
	if mimeType, err := normalizeClipboardMIME("image/png"); err != nil || mimeType != "image/png" {
		t.Fatalf("normalize image/png = %q, %v", mimeType, err)
	}
	for _, value := range []string{"text/html", "image/svg+xml", "text/plain;charset=latin1"} {
		if _, err := normalizeClipboardMIME(value); err == nil {
			t.Fatalf("expected %q to be rejected", value)
		}
	}
}
