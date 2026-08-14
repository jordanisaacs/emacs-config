package bridge

import "testing"

func TestParseWlCopyImage(t *testing.T) {
	options, err := parseWlCopy([]string{"--type", "image/png", "--trim-newline"})
	if err != nil {
		t.Fatal(err)
	}
	if options.mimeType != "image/png" || !options.trimNewline || options.contents != nil {
		t.Fatalf("options = %#v", options)
	}
}

func TestParseWlCopyTextArgument(t *testing.T) {
	options, err := parseWlCopy([]string{"--", "-literal text"})
	if err != nil {
		t.Fatal(err)
	}
	if options.contents == nil || *options.contents != "-literal text" {
		t.Fatalf("options = %#v", options)
	}
}

func TestParseWlPasteImageProbe(t *testing.T) {
	options, err := parseWlPaste([]string{"--list-types", "--no-newline"})
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

func TestWlClipboardRejectsUnsupportedMIME(t *testing.T) {
	if _, err := parseWlCopy([]string{"--type", "text/html"}); err == nil {
		t.Fatal("expected wl-copy to reject text/html")
	}
	if _, err := parseWlPaste([]string{"--type", "image/svg+xml"}); err == nil {
		t.Fatal("expected wl-paste to reject image/svg+xml")
	}
}
