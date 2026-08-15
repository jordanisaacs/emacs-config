package bridge

import "strings"

const ghosttyFocusScript = `on run argv
set targetTTY to item 1 of argv
tell application id "com.mitchellh.ghostty"
  set targetTerminal to first terminal whose tty is targetTTY
  focus targetTerminal
end tell
end run`

func validBundleIdentifier(value string) bool {
	if value == "" {
		return true
	}
	if len(value) > 255 || !strings.Contains(value, ".") {
		return false
	}
	for _, character := range value {
		if (character >= 'a' && character <= 'z') ||
			(character >= 'A' && character <= 'Z') ||
			(character >= '0' && character <= '9') ||
			character == '.' || character == '-' {
			continue
		}
		return false
	}
	return true
}

func validTerminalTTY(value string) bool {
	if value == "" {
		return true
	}
	const prefix = "/dev/tty"
	if len(value) > 128 || !strings.HasPrefix(value, prefix) || len(value) == len(prefix) {
		return false
	}
	for _, character := range value[len(prefix):] {
		if (character >= 'a' && character <= 'z') ||
			(character >= 'A' && character <= 'Z') ||
			(character >= '0' && character <= '9') ||
			character == '.' || character == '_' || character == '-' {
			continue
		}
		return false
	}
	return true
}

func shellQuote(value string) string {
	return "'" + strings.ReplaceAll(value, "'", `'"'"'`) + "'"
}

func ghosttyFocusCommand(tty string) string {
	return strings.Join([]string{
		"/usr/bin/osascript",
		"-e", shellQuote(ghosttyFocusScript),
		"--", shellQuote(tty),
	}, " ")
}

func terminalNotifierArguments(notification notificationRequest) []string {
	arguments := []string{
		"-title", notification.Title,
		"-message", notification.Body,
	}
	if notification.AppName != "" {
		arguments = append(arguments, "-subtitle", notification.AppName)
	}
	if notification.FocusTTY != "" {
		arguments = append(arguments, "-execute", ghosttyFocusCommand(notification.FocusTTY))
	} else if notification.FocusBundleID != "" {
		arguments = append(arguments, "-activate", notification.FocusBundleID)
	}
	return arguments
}
