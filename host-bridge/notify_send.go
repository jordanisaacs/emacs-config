package bridge

import (
	"errors"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
)

const focusBundleHint = "string:x-hostctl-focus-bundle:"
const focusTTYHint = "string:x-hostctl-focus-tty:"

func parseNotifySend(args []string) (notificationRequest, bool, error) {
	var request notificationRequest
	var positional []string
	for position := 0; position < len(args); position++ {
		argument := args[position]
		if argument == "--" {
			positional = append(positional, args[position+1:]...)
			break
		}
		if !strings.HasPrefix(argument, "-") || argument == "-" {
			positional = append(positional, args[position:]...)
			break
		}

		name, inlineValue, hasInlineValue := strings.Cut(argument, "=")
		switch name {
		case "--help", "-?":
			return notificationRequest{}, true, nil
		case "--version":
			return notificationRequest{}, true, nil
		case "--urgency", "-u":
			value, next, err := optionValue(args, position, inlineValue, hasInlineValue)
			if err != nil {
				return request, false, fmt.Errorf("%s: %w", name, err)
			}
			request.Urgency = value
			position = next
		case "--expire-time", "-t":
			value, next, err := optionValue(args, position, inlineValue, hasInlineValue)
			if err != nil {
				return request, false, fmt.Errorf("%s: %w", name, err)
			}
			milliseconds, err := strconv.Atoi(value)
			if err != nil || milliseconds < 0 {
				return request, false, errors.New("expire time must be a non-negative integer")
			}
			request.ExpireTime = milliseconds
			position = next
		case "--app-name", "-a":
			value, next, err := optionValue(args, position, inlineValue, hasInlineValue)
			if err != nil {
				return request, false, fmt.Errorf("%s: %w", name, err)
			}
			request.AppName = value
			position = next
		case "--icon", "-i", "--category", "-c":
			_, next, err := optionValue(args, position, inlineValue, hasInlineValue)
			if err != nil {
				return request, false, fmt.Errorf("%s: %w", name, err)
			}
			position = next
		case "--hint", "-h":
			value, next, err := optionValue(args, position, inlineValue, hasInlineValue)
			if err != nil {
				return request, false, fmt.Errorf("%s: %w", name, err)
			}
			if strings.HasPrefix(value, focusBundleHint) {
				request.FocusBundleID = strings.TrimPrefix(value, focusBundleHint)
			} else if strings.HasPrefix(value, focusTTYHint) {
				request.FocusTTY = strings.TrimPrefix(value, focusTTYHint)
			}
			position = next
		default:
			return request, false, fmt.Errorf("unsupported notify-send option: %s", argument)
		}
	}

	if len(positional) == 0 || len(positional) > 2 {
		return request, false, errors.New("notify-send requires SUMMARY and optional BODY")
	}
	request.Title = positional[0]
	if len(positional) == 2 {
		request.Body = positional[1]
	}
	return request, false, nil
}

func notificationFocusBundleIDFromEnvironment() string {
	if configured := strings.TrimSpace(os.Getenv("HOSTCTL_NOTIFICATION_FOCUS_BUNDLE_ID")); configured != "" {
		return configured
	}
	switch strings.ToLower(strings.TrimSpace(os.Getenv("TERM_PROGRAM"))) {
	case "ghostty":
		return "com.mitchellh.ghostty"
	case "apple_terminal":
		return "com.apple.Terminal"
	case "iterm.app", "iterm2":
		return "com.googlecode.iterm2"
	case "wezterm":
		return "com.github.wez.wezterm"
	case "kitty":
		return "net.kovidgoyal.kitty"
	default:
		return ""
	}
}

func notificationFocusTTYFromEnvironment() string {
	return strings.TrimSpace(os.Getenv("HOSTCTL_NOTIFICATION_FOCUS_TTY"))
}

func optionValue(
	args []string,
	position int,
	inlineValue string,
	hasInlineValue bool,
) (string, int, error) {
	if hasInlineValue {
		if inlineValue == "" {
			return "", position, errors.New("option value may not be empty")
		}
		return inlineValue, position, nil
	}
	if position+1 >= len(args) {
		return "", position, errors.New("option requires a value")
	}
	return args[position+1], position + 1, nil
}

func printNotifySendHelp(writer io.Writer) {
	_, _ = fmt.Fprintln(writer, `Usage:
  notify-send [OPTIONS] <summary> [body]

Options:
  -u, --urgency=LEVEL        Urgency level
  -t, --expire-time=TIME     Expiration timeout in milliseconds
  -a, --app-name=APP         Application name
  -i, --icon=ICON            Accepted for compatibility
  -c, --category=TYPE        Accepted for compatibility
  -h, --hint=HINT            Pass notification metadata
  -?, --help                 Show this help`)
}
