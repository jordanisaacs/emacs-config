package bridge

import (
	"reflect"
	"testing"
)

func TestTerminalNotifierArgumentsIncludeActivationTarget(t *testing.T) {
	request := notificationRequest{
		Title:         "Build finished",
		Body:          "Everything passed",
		AppName:       "Emacs",
		FocusBundleID: "com.mitchellh.ghostty",
		FocusTTY:      "/dev/ttys017",
	}
	want := []string{
		"-title", "Build finished",
		"-message", "Everything passed",
		"-subtitle", "Emacs",
		"-execute", ghosttyFocusCommand("/dev/ttys017"),
	}
	if got := terminalNotifierArguments(request); !reflect.DeepEqual(got, want) {
		t.Fatalf("arguments = %#v, want %#v", got, want)
	}
}

func TestTerminalNotifierArgumentsFallBackToBundleActivation(t *testing.T) {
	request := notificationRequest{
		Title:         "Build finished",
		FocusBundleID: "com.mitchellh.ghostty",
	}
	want := []string{
		"-title", "Build finished",
		"-message", "",
		"-activate", "com.mitchellh.ghostty",
	}
	if got := terminalNotifierArguments(request); !reflect.DeepEqual(got, want) {
		t.Fatalf("arguments = %#v, want %#v", got, want)
	}
}

func TestGhosttyFocusCommandQuotesTTYAsData(t *testing.T) {
	want := "/usr/bin/osascript -e '" + ghosttyFocusScript + "' -- '/dev/ttys017'"
	if got := ghosttyFocusCommand("/dev/ttys017"); got != want {
		t.Fatalf("command = %q, want %q", got, want)
	}
}

func TestValidBundleIdentifier(t *testing.T) {
	for _, value := range []string{"", "com.mitchellh.ghostty", "dev.example.Terminal-Preview"} {
		if !validBundleIdentifier(value) {
			t.Errorf("expected valid bundle identifier: %q", value)
		}
	}
	for _, value := range []string{"ghostty", "com.example.bad value", "com.example.$shell"} {
		if validBundleIdentifier(value) {
			t.Errorf("expected invalid bundle identifier: %q", value)
		}
	}
}

func TestValidTerminalTTY(t *testing.T) {
	for _, value := range []string{"", "/dev/ttys001", "/dev/tty.usbserial-123"} {
		if !validTerminalTTY(value) {
			t.Errorf("expected valid terminal tty: %q", value)
		}
	}
	for _, value := range []string{"ttys001", "/dev/tty", "/dev/ttys001;open /tmp/x", "/dev/ttys/001"} {
		if validTerminalTTY(value) {
			t.Errorf("expected invalid terminal tty: %q", value)
		}
	}
}
