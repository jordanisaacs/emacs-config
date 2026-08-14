package bridge

import "testing"

func TestParseNotifySendSupportsAlertArguments(t *testing.T) {
	request, help, err := parseNotifySend([]string{
		"--icon", "/tmp/emacs.svg",
		"--app-name", "Emacs",
		"--urgency", "critical",
		"--expire-time", "5000",
		"--category", "build",
		"Build finished",
		"Everything passed",
	})
	if err != nil {
		t.Fatal(err)
	}
	if help {
		t.Fatal("unexpected help result")
	}
	if request.Title != "Build finished" || request.Body != "Everything passed" {
		t.Fatalf("request = %#v", request)
	}
	if request.AppName != "Emacs" || request.Urgency != "critical" || request.ExpireTime != 5000 {
		t.Fatalf("request options = %#v", request)
	}
}

func TestParseNotifySendSupportsLongEqualsOptions(t *testing.T) {
	request, _, err := parseNotifySend([]string{
		"--app-name=Ghostel",
		"--urgency=normal",
		"--expire-time=1000",
		"Title",
	})
	if err != nil {
		t.Fatal(err)
	}
	if request.AppName != "Ghostel" || request.Title != "Title" {
		t.Fatalf("request = %#v", request)
	}
}

func TestParseNotifySendRejectsUnknownOption(t *testing.T) {
	if _, _, err := parseNotifySend([]string{"--action", "open=Open", "Title"}); err == nil {
		t.Fatal("expected unsupported option error")
	}
}
