package bridge

import (
	"strings"
	"testing"
)

func TestParseWlCopyImage(t *testing.T) {
	options, err := parseWlCopy([]string{"--type", "image/png", "--trim-newline"})
	if err != nil {
		t.Fatal(err)
	}
	if options.mimeType != "image/png" || !options.trimNewline || len(options.contents) != 0 {
		t.Fatalf("options = %#v", options)
	}
}

func TestParseWlCopyMatchesCommonUpstreamOptions(t *testing.T) {
	options, err := parseWlCopy([]string{"--sensitive", "-fn", "hello", "from", "host"})
	if err != nil {
		t.Fatal(err)
	}
	if got := strings.Join(options.contents, " "); got != "hello from host" {
		t.Fatalf("contents = %q", got)
	}
	if options.mimeType != "" {
		t.Fatalf("default MIME = %q, want inference", options.mimeType)
	}
	if !options.trimNewline {
		t.Fatal("clustered -fn did not enable trim-newline")
	}
}

func TestParseWlCopyTextArgument(t *testing.T) {
	options, err := parseWlCopy([]string{"--", "-literal text"})
	if err != nil {
		t.Fatal(err)
	}
	if len(options.contents) != 1 || options.contents[0] != "-literal text" {
		t.Fatalf("options = %#v", options)
	}
}

func TestInferClipboardMIME(t *testing.T) {
	if got, err := inferClipboardMIME([]byte("hello")); err != nil || got != defaultTextMIME {
		t.Fatalf("text MIME = %q, %v", got, err)
	}
	png := []byte{0x89, 'P', 'N', 'G', '\r', '\n', 0x1a, '\n', 0, 0, 0, 0}
	if got, err := inferClipboardMIME(png); err != nil || got != "image/png" {
		t.Fatalf("PNG MIME = %q, %v", got, err)
	}
	tiff := []byte{'I', 'I', '*', 0, 8, 0, 0, 0}
	if got, err := inferClipboardMIME(tiff); err != nil || got != "image/tiff" {
		t.Fatalf("TIFF MIME = %q, %v", got, err)
	}
}

func TestParseWlPasteImageProbe(t *testing.T) {
	options, err := parseWlPaste([]string{"-ln"})
	if err != nil {
		t.Fatal(err)
	}
	if !options.listTypes || !options.noNewline {
		t.Fatalf("options = %#v", options)
	}

	options, err = parseWlPaste([]string{"--type=image/jpeg"})
	if err != nil {
		t.Fatal(err)
	}
	if options.mimeType != "image/jpeg" {
		t.Fatalf("options = %#v", options)
	}
}

func TestParseWlPasteWatchAndVersion(t *testing.T) {
	options, err := parseWlPaste([]string{"--type", "text", "--watch", "cat", "-n"})
	if err != nil {
		t.Fatal(err)
	}
	if options.mimeType != defaultTextMIME || len(options.watch) != 2 || options.watch[0] != "cat" || options.watch[1] != "-n" {
		t.Fatalf("options = %#v", options)
	}
	options, err = parseWlPaste([]string{"--version"})
	if err != nil || !options.version || options.help {
		t.Fatalf("version options = %#v, %v", options, err)
	}
}

func TestParseWlPasteSave(t *testing.T) {
	options, err := parseWlPaste([]string{"--save", "--no-newline"})
	if err != nil || !options.save || !options.noNewline {
		t.Fatalf("save options = %#v, %v", options, err)
	}
	if _, err := parseWlPaste([]string{"--save", "--list-types"}); err == nil {
		t.Fatal("expected --save and --list-types to conflict")
	}
	if _, err := parseWlPaste([]string{"--save", "--watch", "cat"}); err == nil {
		t.Fatal("expected --save and --watch to conflict")
	}
}

func TestWlClipboardRejectsUnsupportedMIME(t *testing.T) {
	if _, err := parseWlCopy([]string{"--type", "text/html"}); err == nil {
		t.Fatal("expected wl-copy to reject text/html")
	}
	if _, err := parseWlPaste([]string{"--type", "image/svg+xml"}); err == nil {
		t.Fatal("expected wl-paste to reject image/svg+xml")
	}
}
