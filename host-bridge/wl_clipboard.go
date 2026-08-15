package bridge

import (
	"bytes"
	"context"
	"crypto/sha256"
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"strings"
	"time"
	"unicode/utf8"
)

const wlClipboardHostVersion = "0.1.0"

type wlCopyOptions struct {
	mimeType    string
	trimNewline bool
	clear       bool
	contents    []string
	help        bool
	version     bool
}

type wlPasteOptions struct {
	mimeType  string
	listTypes bool
	noNewline bool
	save      bool
	watch     []string
	help      bool
	version   bool
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
	if len(options.contents) != 0 {
		contents = []byte(strings.Join(options.contents, " "))
	} else if !options.clear {
		limit := int64(maxImageClipboardBytes)
		if options.mimeType != "" {
			limit = clipboardLimit(options.mimeType)
		}
		contents, err = readBounded(stdin, limit)
		if err != nil {
			fmt.Fprintln(stderr, err)
			return 1
		}
	}
	if options.trimNewline {
		contents = bytes.TrimSuffix(contents, []byte{'\n'})
	}
	if options.mimeType == "" {
		if len(options.contents) != 0 {
			options.mimeType = defaultTextMIME
		} else {
			options.mimeType, err = inferClipboardMIME(contents)
			if err != nil {
				fmt.Fprintln(stderr, err)
				return 1
			}
		}
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
	if len(options.watch) != 0 {
		return runWlPasteWatch(ctx, client, options, stdout, stderr)
	}
	if options.save {
		return runWlPasteSave(ctx, client, options, stdout, stderr)
	}

	mimeType, contents, state, err := readWlPasteClipboard(ctx, client, options.mimeType)
	if err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	if state == "nil" {
		fmt.Fprintln(stderr, "Nothing is copied")
		return 1
	}
	if !options.noNewline && mimeType == defaultTextMIME {
		contents = append(contents, '\n')
	}
	if _, err := stdout.Write(contents); err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	return 0
}

func parseWlCopy(args []string) (wlCopyOptions, error) {
	options := wlCopyOptions{}
	for position := 0; position < len(args); position++ {
		argument := args[position]
		if argument == "--" {
			options.contents = append(options.contents, args[position+1:]...)
			break
		}
		if !strings.HasPrefix(argument, "-") || argument == "-" {
			options.contents = append(options.contents, argument)
			continue
		}
		if strings.HasPrefix(argument, "-") && !strings.HasPrefix(argument, "--") && len(argument) > 2 {
			stop, err := parseWlCopyShortOptions(args, &position, &options)
			if err != nil {
				return options, err
			}
			if stop {
				return options, nil
			}
			continue
		}

		name, inlineValue, hasInlineValue := strings.Cut(argument, "=")
		switch name {
		case "-h", "--help":
			options.help = true
			return options, nil
		case "-v", "--version":
			options.version = true
			return options, nil
		case "-t", "--type":
			value, next, err := optionValue(args, position, inlineValue, hasInlineValue)
			if err != nil {
				return options, fmt.Errorf("wl-copy: %s: %w", name, err)
			}
			options.mimeType, err = normalizeWlClipboardMIME(value)
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
		case "-p", "--primary", "-o", "--paste-once", "-f", "--foreground", "--sensitive":
			// There is only one host pasteboard and hostd takes ownership of the data.
		default:
			return options, fmt.Errorf("wl-copy: unsupported option: %s", argument)
		}
	}
	if options.clear {
		options.contents = nil
		options.mimeType = defaultTextMIME
	}
	return options, nil
}

func parseWlCopyShortOptions(args []string, position *int, options *wlCopyOptions) (bool, error) {
	argument := args[*position]
	for offset := 1; offset < len(argument); offset++ {
		name := "-" + string(argument[offset])
		switch name {
		case "-h":
			options.help = true
			return true, nil
		case "-v":
			options.version = true
			return true, nil
		case "-n":
			options.trimNewline = true
		case "-c":
			options.clear = true
		case "-p", "-o", "-f":
			// Accepted for compatibility with wl-clipboard.
		case "-t", "-s":
			value := argument[offset+1:]
			if value == "" {
				if *position+1 >= len(args) {
					return false, fmt.Errorf("wl-copy: %s requires an argument", name)
				}
				*position++
				value = args[*position]
			}
			if name == "-t" {
				mimeType, err := normalizeWlClipboardMIME(value)
				if err != nil {
					return false, fmt.Errorf("wl-copy: %w", err)
				}
				options.mimeType = mimeType
			}
			return false, nil
		default:
			return false, fmt.Errorf("wl-copy: unsupported option: %s", name)
		}
	}
	return false, nil
}

func parseWlPaste(args []string) (wlPasteOptions, error) {
	options := wlPasteOptions{}
	for position := 0; position < len(args); position++ {
		argument := args[position]
		if strings.HasPrefix(argument, "-") && !strings.HasPrefix(argument, "--") && len(argument) > 2 {
			stop, err := parseWlPasteShortOptions(args, &position, &options)
			if err != nil {
				return options, err
			}
			if stop {
				return validateWlPasteOptions(options)
			}
			continue
		}
		name, inlineValue, hasInlineValue := strings.Cut(argument, "=")
		switch name {
		case "-h", "--help":
			options.help = true
			return options, nil
		case "-v", "--version":
			options.version = true
			return options, nil
		case "-l", "--list-types":
			options.listTypes = true
		case "-n", "--no-newline":
			options.noNewline = true
		case "--save":
			options.save = true
		case "-t", "--type":
			value, next, err := optionValue(args, position, inlineValue, hasInlineValue)
			if err != nil {
				return options, fmt.Errorf("wl-paste: %s: %w", name, err)
			}
			options.mimeType, err = normalizeWlClipboardMIME(value)
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
		case "-w", "--watch":
			if hasInlineValue || position+1 >= len(args) {
				return options, fmt.Errorf("wl-paste: %s requires a command argument", name)
			}
			options.watch = append([]string(nil), args[position+1:]...)
			return validateWlPasteOptions(options)
		case "-p", "--primary":
			// There is only one host pasteboard.
		default:
			return options, fmt.Errorf("wl-paste: unsupported option: %s", argument)
		}
	}
	return validateWlPasteOptions(options)
}

func validateWlPasteOptions(options wlPasteOptions) (wlPasteOptions, error) {
	if options.save && options.listTypes {
		return options, fmt.Errorf("wl-paste: --save cannot be combined with --list-types")
	}
	if options.save && len(options.watch) != 0 {
		return options, fmt.Errorf("wl-paste: --save cannot be combined with --watch")
	}
	return options, nil
}

func parseWlPasteShortOptions(args []string, position *int, options *wlPasteOptions) (bool, error) {
	argument := args[*position]
	for offset := 1; offset < len(argument); offset++ {
		name := "-" + string(argument[offset])
		switch name {
		case "-h":
			options.help = true
			return true, nil
		case "-v":
			options.version = true
			return true, nil
		case "-n":
			options.noNewline = true
		case "-l":
			options.listTypes = true
		case "-p":
			// There is only one host pasteboard.
		case "-t", "-s":
			value := argument[offset+1:]
			if value == "" {
				if *position+1 >= len(args) {
					return false, fmt.Errorf("wl-paste: %s requires an argument", name)
				}
				*position++
				value = args[*position]
			}
			if name == "-t" {
				mimeType, err := normalizeWlClipboardMIME(value)
				if err != nil {
					return false, fmt.Errorf("wl-paste: %w", err)
				}
				options.mimeType = mimeType
			}
			return false, nil
		case "-w":
			if offset+1 != len(argument) || *position+1 >= len(args) {
				return false, fmt.Errorf("wl-paste: -w requires a separate command argument")
			}
			options.watch = append([]string(nil), args[*position+1:]...)
			return true, nil
		default:
			return false, fmt.Errorf("wl-paste: unsupported option: %s", name)
		}
	}
	return false, nil
}

func normalizeWlClipboardMIME(value string) (string, error) {
	if value == "text" {
		return defaultTextMIME, nil
	}
	return normalizeClipboardMIME(value)
}

func inferClipboardMIME(contents []byte) (string, error) {
	if len(contents) >= 4 {
		if bytes.Equal(contents[:4], []byte{'I', 'I', '*', 0}) ||
			bytes.Equal(contents[:4], []byte{'M', 'M', 0, '*'}) {
			return "image/tiff", nil
		}
	}
	detected := http.DetectContentType(contents)
	if mimeType, err := normalizeClipboardMIME(detected); err == nil && mimeType != defaultTextMIME {
		return mimeType, nil
	}
	if utf8.Valid(contents) {
		return defaultTextMIME, nil
	}
	return "", fmt.Errorf("wl-copy: cannot infer a supported clipboard MIME type (detected %s); use --type", detected)
}

func readWlPasteClipboard(
	ctx context.Context,
	client *bridgeClient,
	requestedType string,
) (string, []byte, string, error) {
	types, err := client.clipboardTypes(ctx)
	if err != nil {
		return "", nil, "", err
	}
	mimeType, err := chooseWlPasteMIME(types, requestedType)
	if err != nil {
		return "", nil, "", err
	}
	if mimeType == "" {
		return "", nil, "nil", nil
	}
	contents, err := client.readClipboard(ctx, mimeType)
	if err != nil {
		return "", nil, "", err
	}
	return mimeType, contents, "data", nil
}

func chooseWlPasteMIME(types []string, requestedType string) (string, error) {
	if len(types) == 0 {
		return "", nil
	}
	var firstSupported string
	for _, offered := range types {
		normalized, err := normalizeClipboardMIME(offered)
		if err != nil {
			continue
		}
		if firstSupported == "" {
			firstSupported = normalized
		}
		if requestedType == "" && normalized == defaultTextMIME {
			return normalized, nil
		}
		if requestedType != "" && normalized == requestedType {
			return normalized, nil
		}
	}
	if requestedType == "" && firstSupported != "" {
		return firstSupported, nil
	}
	if requestedType == "" {
		return "", fmt.Errorf("wl-paste: clipboard has no supported content type")
	}
	return "", fmt.Errorf("wl-paste: clipboard content is not available as requested type %q", requestedType)
}

func runWlPasteSave(
	ctx context.Context,
	client *bridgeClient,
	options wlPasteOptions,
	stdout, stderr io.Writer,
) int {
	if options.mimeType == "" {
		archive, err := client.clipboardFiles(ctx)
		if err == nil {
			paths, extractErr := materializeClipboardArchive(archive)
			closeErr := archive.Close()
			if extractErr == nil {
				extractErr = closeErr
			}
			if extractErr != nil {
				fmt.Fprintln(stderr, extractErr)
				return 1
			}
			return writeWlPastePaths(stdout, stderr, paths, options.noNewline)
		}
		if !errors.Is(err, errClipboardHasNoFiles) {
			fmt.Fprintln(stderr, err)
			return 1
		}
	}

	mimeType, contents, err := readWlPasteImage(ctx, client, options.mimeType)
	if err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	imagePath, err := materializeClipboardImage(mimeType, contents)
	if err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	return writeWlPastePaths(stdout, stderr, []string{imagePath}, options.noNewline)
}

func readWlPasteImage(
	ctx context.Context,
	client *bridgeClient,
	requestedType string,
) (string, []byte, error) {
	if requestedType == defaultTextMIME {
		return "", nil, errors.New("wl-paste: --save requires an image, file, or folder clipboard")
	}
	types, err := client.clipboardTypes(ctx)
	if err != nil {
		return "", nil, err
	}
	var selected string
	for _, offered := range types {
		normalized, err := normalizeClipboardMIME(offered)
		if err != nil || normalized == defaultTextMIME {
			continue
		}
		if selected == "" {
			selected = normalized
		}
		if requestedType != "" && normalized == requestedType {
			selected = normalized
			break
		}
		if requestedType == "" && normalized == "image/png" {
			selected = normalized
			break
		}
	}
	if selected == "" || (requestedType != "" && selected != requestedType) {
		return "", nil, errors.New("wl-paste: clipboard has no image, file, or folder to save")
	}
	contents, err := client.readClipboard(ctx, selected)
	if err != nil {
		return "", nil, err
	}
	return selected, contents, nil
}

func writeWlPastePaths(stdout, stderr io.Writer, paths []string, noNewline bool) int {
	for index, savedPath := range paths {
		if index != 0 {
			if _, err := io.WriteString(stdout, "\n"); err != nil {
				fmt.Fprintln(stderr, err)
				return 1
			}
		}
		if _, err := io.WriteString(stdout, savedPath); err != nil {
			fmt.Fprintln(stderr, err)
			return 1
		}
	}
	if !noNewline {
		if _, err := io.WriteString(stdout, "\n"); err != nil {
			fmt.Fprintln(stderr, err)
			return 1
		}
	}
	return 0
}

func runWlPasteWatch(
	ctx context.Context,
	client *bridgeClient,
	options wlPasteOptions,
	stdout, stderr io.Writer,
) int {
	var previous [sha256.Size]byte
	havePrevious := false
	ticker := time.NewTicker(500 * time.Millisecond)
	defer ticker.Stop()
	for {
		mimeType, contents, state, err := readWlPasteClipboard(ctx, client, options.mimeType)
		if err != nil {
			fmt.Fprintln(stderr, err)
			return 1
		}
		digestInput := append([]byte(mimeType+"\x00"+state+"\x00"), contents...)
		digest := sha256.Sum256(digestInput)
		if !havePrevious || digest != previous {
			command := exec.CommandContext(ctx, options.watch[0], options.watch[1:]...)
			command.Stdin = bytes.NewReader(contents)
			command.Stdout = stdout
			command.Stderr = stderr
			command.Env = append(os.Environ(), "CLIPBOARD_STATE="+state)
			if err := command.Run(); err != nil {
				fmt.Fprintf(stderr, "wl-paste: watch command failed: %v\n", err)
				return 1
			}
			previous = digest
			havePrevious = true
		}
		select {
		case <-ctx.Done():
			return 1
		case <-ticker.C:
		}
	}
}

func printWlClipboardVersion(writer io.Writer) {
	_, _ = fmt.Fprintf(writer, "wl-clipboard host bridge %s\n", wlClipboardHostVersion)
}

func printWlCopyHelp(writer io.Writer) {
	_, _ = fmt.Fprintln(writer, `Usage:
	wl-copy [options] text to copy
	wl-copy [options] < file-to-copy

Copy content to the connected host clipboard.

Options:
	-o, --paste-once	Only serve one paste request and then exit.
	-f, --foreground	Stay in the foreground instead of forking.
	-c, --clear		Instead of copying, clear the clipboard.
	-p, --primary		Use the "primary" clipboard.
	-n, --trim-newline	Do not copy the trailing newline character.
	-t, --type mime/type	Override the inferred MIME type for the content.
	    --sensitive		Hint that the content is sensitive.
	-s, --seat seat-name	Pick the seat to work with.
	-v, --version		Display version info.
	-h, --help		Display this message.
Mandatory arguments to long options are mandatory for short options too.`)
}

func printWlPasteHelp(writer io.Writer) {
	_, _ = fmt.Fprintln(writer, `Usage:
	wl-paste [options]

Paste content from the connected host clipboard.

Options:
	-n, --no-newline	Do not append a newline character.
	-l, --list-types	Instead of pasting, list the offered types.
	    --save		Save an image or copied files under /tmp and print path(s).
	-p, --primary		Use the "primary" clipboard.
	-w, --watch command	Run a command each time the selection changes.
	-t, --type mime/type	Override the inferred MIME type for the content.
	-s, --seat seat-name	Pick the seat to work with.
	-v, --version		Display version info.
	-h, --help		Display this message.
Mandatory arguments to long options are mandatory for short options too.`)
}
