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

func TestParseNotifySendPreservesFocusBundleHint(t *testing.T) {
	request, _, err := parseNotifySend([]string{
		"--hint", focusBundleHint + "com.mitchellh.ghostty",
		"Title",
	})
	if err != nil {
		t.Fatal(err)
	}
	if request.FocusBundleID != "com.mitchellh.ghostty" {
		t.Fatalf("focus bundle = %q", request.FocusBundleID)
	}
}

func TestParseNotifySendPreservesFocusTTYHint(t *testing.T) {
	request, _, err := parseNotifySend([]string{
		"--hint", focusTTYHint + "/dev/ttys017",
		"Title",
	})
	if err != nil {
		t.Fatal(err)
	}
	if request.FocusTTY != "/dev/ttys017" {
		t.Fatalf("focus tty = %q", request.FocusTTY)
	}
}

func TestNotificationFocusBundleIDFromEnvironment(t *testing.T) {
	t.Setenv("HOSTCTL_NOTIFICATION_FOCUS_BUNDLE_ID", "")
	t.Setenv("TERM_PROGRAM", "ghostty")
	if bundleID := notificationFocusBundleIDFromEnvironment(); bundleID != "com.mitchellh.ghostty" {
		t.Fatalf("focus bundle = %q", bundleID)
	}

	t.Setenv("HOSTCTL_NOTIFICATION_FOCUS_BUNDLE_ID", "dev.example.Terminal")
	if bundleID := notificationFocusBundleIDFromEnvironment(); bundleID != "dev.example.Terminal" {
		t.Fatalf("configured focus bundle = %q", bundleID)
	}
}

func TestNotificationFocusTTYFromEnvironment(t *testing.T) {
	t.Setenv("HOSTCTL_NOTIFICATION_FOCUS_TTY", " /dev/ttys017 ")
	if tty := notificationFocusTTYFromEnvironment(); tty != "/dev/ttys017" {
		t.Fatalf("focus tty = %q", tty)
	}
}

func TestParseNotifySendRejectsUnknownOption(t *testing.T) {
	if _, _, err := parseNotifySend([]string{"--action", "open=Open", "Title"}); err == nil {
		t.Fatal("expected unsupported option error")
	}
}
