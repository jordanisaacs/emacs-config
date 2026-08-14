package bridge

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"strings"
)

type wlCopyOptions struct {
	mimeType    string
	trimNewline bool
	clear       bool
	contents    *string
	help        bool
}

type wlPasteOptions struct {
	mimeType  string
	listTypes bool
	noNewline bool
	help      bool
}

func runWlCopy(
	ctx context.Context,
	client *bridgeClient,
	options wlCopyOptions,
	stdin io.Reader,
	stderr io.Writer,
) int {
	var contents []byte
	var err error
	if options.contents != nil {
		contents = []byte(*options.contents)
	} else if !options.clear {
		contents, err = readBounded(stdin, clipboardLimit(options.mimeType))
		if err != nil {
			fmt.Fprintln(stderr, err)
			return 1
		}
	}
	if options.trimNewline {
		contents = bytes.TrimSuffix(contents, []byte{'\n'})
	}
	if err := client.writeClipboard(ctx, options.mimeType, contents); err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	return 0
}

func runWlPaste(
	ctx context.Context,
	client *bridgeClient,
	options wlPasteOptions,
	stdout, stderr io.Writer,
) int {
	if options.listTypes {
		types, err := client.clipboardTypes(ctx)
		if err != nil {
			fmt.Fprintln(stderr, err)
			return 1
		}
		for _, mimeType := range types {
			fmt.Fprintln(stdout, mimeType)
		}
		return 0
	}

	contents, err := client.readClipboard(ctx, options.mimeType)
	if err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	if !options.noNewline && options.mimeType == defaultTextMIME && !bytes.HasSuffix(contents, []byte{'\n'}) {
		contents = append(contents, '\n')
	}
	if _, err := stdout.Write(contents); err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	return 0
}

func parseWlCopy(args []string) (wlCopyOptions, error) {
	options := wlCopyOptions{mimeType: defaultTextMIME}
	for position := 0; position < len(args); position++ {
		argument := args[position]
		if argument == "--" {
			if err := setWlCopyContents(&options, args[position+1:]); err != nil {
				return options, err
			}
			break
		}
		if !strings.HasPrefix(argument, "-") || argument == "-" {
			if err := setWlCopyContents(&options, args[position:]); err != nil {
				return options, err
			}
			break
		}

		name, inlineValue, hasInlineValue := strings.Cut(argument, "=")
		switch name {
		case "-h", "--help", "--version":
			options.help = true
			return options, nil
		case "-t", "--type":
			value, next, err := optionValue(args, position, inlineValue, hasInlineValue)
			if err != nil {
				return options, fmt.Errorf("wl-copy: %s: %w", name, err)
			}
			options.mimeType, err = normalizeClipboardMIME(value)
			if err != nil {
				return options, fmt.Errorf("wl-copy: %w", err)
			}
			position = next
		case "-s", "--seat":
			_, next, err := optionValue(args, position, inlineValue, hasInlineValue)
			if err != nil {
				return options, fmt.Errorf("wl-copy: %s: %w", name, err)
			}
			position = next
		case "-n", "--trim-newline":
			options.trimNewline = true
		case "-c", "--clear":
			options.clear = true
		case "-p", "--primary", "-o", "--paste-once", "-f", "--foreground":
			// There is only one host pasteboard and hostd takes ownership of the data.
		default:
			return options, fmt.Errorf("wl-copy: unsupported option: %s", argument)
		}
	}
	if options.clear {
		if options.contents != nil {
			return options, errors.New("wl-copy: --clear does not accept text")
		}
		options.mimeType = defaultTextMIME
	}
	return options, nil
}

func setWlCopyContents(options *wlCopyOptions, positional []string) error {
	if len(positional) > 1 {
		return errors.New("wl-copy: accepts at most one text argument")
	}
	if len(positional) == 1 {
		options.contents = &positional[0]
	}
	return nil
}

func parseWlPaste(args []string) (wlPasteOptions, error) {
	options := wlPasteOptions{mimeType: defaultTextMIME}
	for position := 0; position < len(args); position++ {
		argument := args[position]
		name, inlineValue, hasInlineValue := strings.Cut(argument, "=")
		switch name {
		case "-h", "--help", "-v", "--version":
			options.help = true
			return options, nil
		case "-l", "--list-types":
			options.listTypes = true
		case "-n", "--no-newline":
			options.noNewline = true
		case "-t", "--type":
			value, next, err := optionValue(args, position, inlineValue, hasInlineValue)
			if err != nil {
				return options, fmt.Errorf("wl-paste: %s: %w", name, err)
			}
			options.mimeType, err = normalizeClipboardMIME(value)
			if err != nil {
				return options, fmt.Errorf("wl-paste: %w", err)
			}
			position = next
		case "-s", "--seat":
			_, next, err := optionValue(args, position, inlineValue, hasInlineValue)
			if err != nil {
				return options, fmt.Errorf("wl-paste: %s: %w", name, err)
			}
			position = next
		case "-p", "--primary":
			// There is only one host pasteboard.
		default:
			return options, fmt.Errorf("wl-paste: unsupported option: %s", argument)
		}
	}
	return options, nil
}

func printWlCopyHelp(writer io.Writer) {
	_, _ = fmt.Fprintln(writer, `Usage: wl-copy [OPTIONS] [TEXT]

Copy UTF-8 text or image data from stdin to the host clipboard.

  -t, --type MIME       Content type (text/plain or a common image type)
  -n, --trim-newline    Remove one trailing newline
  -c, --clear           Clear the host clipboard
  -p, --primary         Accepted for compatibility`)
}

func printWlPasteHelp(writer io.Writer) {
	_, _ = fmt.Fprintln(writer, `Usage: wl-paste [OPTIONS]

Read UTF-8 text or image data from the host clipboard.

  -l, --list-types      List available MIME types
  -t, --type MIME       Request a specific content type
  -n, --no-newline      Do not append a newline to text
  -p, --primary         Accepted for compatibility`)
}
