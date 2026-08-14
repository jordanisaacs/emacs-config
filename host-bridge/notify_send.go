package bridge

import (
	"errors"
	"fmt"
	"io"
	"strconv"
	"strings"
)

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
		case "--icon", "-i", "--category", "-c", "--hint", "-h":
			_, next, err := optionValue(args, position, inlineValue, hasInlineValue)
			if err != nil {
				return request, false, fmt.Errorf("%s: %w", name, err)
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
  -h, --hint=HINT            Accepted for compatibility
  -?, --help                 Show this help`)
}
