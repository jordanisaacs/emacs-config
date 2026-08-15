package bridge

import (
	"bytes"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestInstalledShimCommands(t *testing.T) {
	token := strings.Repeat("a", 32)
	host := &fakeHost{
		clipboard:      []byte("from host"),
		clipboardTypes: []string{defaultTextMIME, "image/png"},
	}
	server := httptest.NewServer(&bridgeHandler{
		token:              token,
		allowClipboardRead: true,
		host:               host,
	})
	defer server.Close()
	tokenFile := filepath.Join(t.TempDir(), "token")
	if err := os.WriteFile(tokenFile, []byte(token+"\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	t.Setenv("EMACS_HOST_BRIDGE_TOKEN_FILE", tokenFile)
	t.Setenv("EMACS_HOST_BRIDGE_URL", server.URL)
	t.Setenv("TERM_PROGRAM", "ghostty")
	t.Setenv("HOSTCTL_NOTIFICATION_FOCUS_BUNDLE_ID", "")
	t.Setenv("HOSTCTL_NOTIFICATION_FOCUS_TTY", "/dev/ttys017")

	var stdout, stderr bytes.Buffer
	exitCode := RunHostctl("/nix/store/example/bin/notify-send", []string{
		"--icon", "/tmp/emacs.svg",
		"--app-name", "Emacs",
		"--urgency", "critical",
		"--expire-time", "5000",
		"--category", "build",
		"Build finished",
		"Everything passed",
	}, strings.NewReader(""), &stdout, &stderr)
	if exitCode != 0 {
		t.Fatalf("notify-send exit = %d, stderr = %q", exitCode, stderr.String())
	}
	if host.notification.Title != "Build finished" || host.notification.AppName != "Emacs" || host.notification.FocusBundleID != "com.mitchellh.ghostty" || host.notification.FocusTTY != "/dev/ttys017" {
		t.Fatalf("notification = %#v", host.notification)
	}

	stderr.Reset()
	exitCode = RunHostctl("xdg-open", []string{"https://example.com/path?q=1"}, strings.NewReader(""), &stdout, &stderr)
	if exitCode != 0 || host.openedURL != "https://example.com/path?q=1" {
		t.Fatalf("xdg-open exit = %d, URL = %q, stderr = %q", exitCode, host.openedURL, stderr.String())
	}

	stderr.Reset()
	exitCode = RunHostctl("wl-copy", []string{"--type", "image/png"}, bytes.NewReader([]byte{0x89, 'P', 'N', 'G'}), &stdout, &stderr)
	if exitCode != 0 || host.writtenMIME != "image/png" {
		t.Fatalf("wl-copy exit = %d, MIME = %q, stderr = %q", exitCode, host.writtenMIME, stderr.String())
	}

	stderr.Reset()
	png := []byte{0x89, 'P', 'N', 'G', '\r', '\n', 0x1a, '\n', 0, 0, 0, 0}
	exitCode = RunHostctl("wl-copy", nil, bytes.NewReader(png), &stdout, &stderr)
	if exitCode != 0 || host.writtenMIME != "image/png" {
		t.Fatalf("wl-copy inferred exit = %d, MIME = %q, stderr = %q", exitCode, host.writtenMIME, stderr.String())
	}

	stdout.Reset()
	stderr.Reset()
	exitCode = RunHostctl("wl-paste", []string{"--no-newline"}, strings.NewReader(""), &stdout, &stderr)
	if exitCode != 0 || stdout.String() != "from host" {
		t.Fatalf("wl-paste exit = %d, stdout = %q, stderr = %q", exitCode, stdout.String(), stderr.String())
	}

	stdout.Reset()
	stderr.Reset()
	exitCode = RunHostctl("wl-paste", nil, strings.NewReader(""), &stdout, &stderr)
	if exitCode != 0 || stdout.String() != "from host\n" {
		t.Fatalf("wl-paste default exit = %d, stdout = %q, stderr = %q", exitCode, stdout.String(), stderr.String())
	}

	stdout.Reset()
	stderr.Reset()
	exitCode = RunHostctl("wl-paste", []string{"--list-types"}, strings.NewReader(""), &stdout, &stderr)
	if exitCode != 0 || stdout.String() != defaultTextMIME+"\nimage/png\n" {
		t.Fatalf("wl-paste -l exit = %d, stdout = %q, stderr = %q", exitCode, stdout.String(), stderr.String())
	}
}

func TestShimHelpDoesNotRequireBridgeConfiguration(t *testing.T) {
	t.Setenv("EMACS_HOST_BRIDGE_TOKEN_FILE", filepath.Join(t.TempDir(), "missing"))
	for _, test := range []struct {
		program string
		args    []string
	}{
		{program: "notify-send", args: []string{"--help"}},
		{program: "xdg-open", args: []string{"--help"}},
		{program: "wl-copy", args: []string{"--help"}},
		{program: "wl-paste", args: []string{"--help"}},
		{program: "wl-copy", args: []string{"--version"}},
		{program: "wl-paste", args: []string{"--version"}},
	} {
		var stdout, stderr bytes.Buffer
		exitCode := RunHostctl(test.program, test.args, strings.NewReader(""), &stdout, &stderr)
		if exitCode != 0 || stdout.Len() == 0 {
			t.Errorf("%s --help exit = %d, stdout = %q, stderr = %q", test.program, exitCode, stdout.String(), stderr.String())
		}
	}
}

func TestWlPasteSaveImage(t *testing.T) {
	png := []byte{0x89, 'P', 'N', 'G', '\r', '\n', 0x1a, '\n'}
	host := &fakeHost{clipboard: png, clipboardTypes: []string{"image/png", "image/tiff"}}
	server, tokenFile := testClipboardServer(t, host)
	t.Setenv("EMACS_HOST_BRIDGE_TOKEN_FILE", tokenFile)
	t.Setenv("EMACS_HOST_BRIDGE_URL", server.URL)

	var stdout, stderr bytes.Buffer
	exitCode := RunHostctl("wl-paste", []string{"--save", "--no-newline"}, strings.NewReader(""), &stdout, &stderr)
	if exitCode != 0 {
		t.Fatalf("exit = %d, stderr = %q", exitCode, stderr.String())
	}
	path := stdout.String()
	t.Cleanup(func() { _ = os.RemoveAll(filepath.Dir(path)) })
	contents, err := os.ReadFile(path)
	if err != nil || !bytes.Equal(contents, png) {
		t.Fatalf("saved image = %v, %v", contents, err)
	}
}

func TestWlPasteSaveCopiedFolder(t *testing.T) {
	sourceRoot := t.TempDir()
	folder := filepath.Join(sourceRoot, "copied-folder")
	if err := os.Mkdir(folder, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(folder, "file.txt"), []byte("transferred"), 0o600); err != nil {
		t.Fatal(err)
	}
	host := &fakeHost{clipboardFiles: []string{folder}}
	server, tokenFile := testClipboardServer(t, host)
	t.Setenv("EMACS_HOST_BRIDGE_TOKEN_FILE", tokenFile)
	t.Setenv("EMACS_HOST_BRIDGE_URL", server.URL)

	var stdout, stderr bytes.Buffer
	exitCode := RunHostctl("wl-paste", []string{"--save"}, strings.NewReader(""), &stdout, &stderr)
	if exitCode != 0 {
		t.Fatalf("exit = %d, stderr = %q", exitCode, stderr.String())
	}
	savedFolder := strings.TrimSpace(stdout.String())
	t.Cleanup(func() { _ = os.RemoveAll(filepath.Dir(savedFolder)) })
	contents, err := os.ReadFile(filepath.Join(savedFolder, "file.txt"))
	if err != nil || string(contents) != "transferred" {
		t.Fatalf("saved file = %q, %v", contents, err)
	}
}

func testClipboardServer(t *testing.T, host *fakeHost) (*httptest.Server, string) {
	t.Helper()
	token := strings.Repeat("b", 32)
	server := httptest.NewServer(&bridgeHandler{
		token:              token,
		allowClipboardRead: true,
		host:               host,
	})
	t.Cleanup(server.Close)
	tokenFile := filepath.Join(t.TempDir(), "token")
	if err := os.WriteFile(tokenFile, []byte(token+"\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	return server, tokenFile
}
