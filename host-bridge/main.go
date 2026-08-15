package bridge

import (
	"context"
	"flag"
	"fmt"
	"io"
	"path/filepath"
	"strings"
)

// RunHostd runs the macOS host daemon command and returns its process exit code.
func RunHostd(args []string, stdout, stderr io.Writer) int {
	if len(args) == 0 || args[0] == "serve" || strings.HasPrefix(args[0], "-") {
		if len(args) > 0 && args[0] == "serve" {
			args = args[1:]
		}
		return runServe(args, stderr)
	}
	switch args[0] {
	case "init-token":
		return runInitToken(args[1:], stdout, stderr)
	case "help", "--help", "-h":
		printHostdUsage(stdout)
		return 0
	default:
		fmt.Fprintf(stderr, "unknown hostd command: %s\n", args[0])
		printHostdUsage(stderr)
		return 2
	}
}

// RunHostctl runs the remote client command and returns its process exit code.
func RunHostctl(program string, args []string, stdin io.Reader, stdout, stderr io.Writer) int {
	program = filepath.Base(program)
	if program == "notify-send" || program == "xdg-open" || program == "wl-copy" || program == "wl-paste" {
		return runClientCommand(program, args, stdin, stdout, stderr)
	}
	if len(args) == 0 {
		printHostctlUsage(stderr)
		return 2
	}
	command := args[0]
	commandArgs := args[1:]
	switch command {
	case "notify", "notify-send", "open", "xdg-open", "wl-copy", "wl-paste", "clipboard-write", "clipboard-read", "health":
		return runClientCommand(command, commandArgs, stdin, stdout, stderr)
	case "help", "--help", "-h":
		printHostctlUsage(stdout)
		return 0
	default:
		fmt.Fprintf(stderr, "unknown command: %s\n", command)
		printHostctlUsage(stderr)
		return 2
	}
}

func runServe(args []string, stderr io.Writer) int {
	flags := flag.NewFlagSet("serve", flag.ContinueOnError)
	flags.SetOutput(stderr)
	listen := flags.String("listen", defaultListenAddress, "loopback address to listen on")
	tokenFileDefault, err := defaultTokenFile()
	if err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	tokenFile := flags.String("token-file", tokenFileDefault, "shared bearer-token file")
	allowClipboardRead := flags.Bool("allow-clipboard-read", false, "allow the remote to read host clipboard contents")
	if err := flags.Parse(args); err != nil {
		return 2
	}
	if flags.NArg() != 0 {
		fmt.Fprintln(stderr, "serve does not accept positional arguments")
		return 2
	}
	token, err := readToken(*tokenFile)
	if err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	host, err := newPlatformHostActions()
	if err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	if err := serve(*listen, token, *allowClipboardRead, host); err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	return 0
}

func runInitToken(args []string, stdout, stderr io.Writer) int {
	flags := flag.NewFlagSet("init-token", flag.ContinueOnError)
	flags.SetOutput(stderr)
	tokenFileDefault, err := defaultTokenFile()
	if err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	tokenFile := flags.String("token-file", tokenFileDefault, "shared bearer-token file")
	if err := flags.Parse(args); err != nil {
		return 2
	}
	if flags.NArg() != 0 {
		fmt.Fprintln(stderr, "init-token does not accept positional arguments")
		return 2
	}
	if err := initializeToken(*tokenFile); err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	fmt.Fprintln(stdout, *tokenFile)
	return 0
}

func runClientCommand(
	command string,
	args []string,
	stdin io.Reader,
	stdout, stderr io.Writer,
) int {
	var client *bridgeClient
	getClient := func() (*bridgeClient, error) {
		if client != nil {
			return client, nil
		}
		var err error
		client, err = newBridgeClientFromEnvironment()
		return client, err
	}
	ctx := context.Background()
	switch command {
	case "notify", "notify-send":
		notification, help, err := parseNotifySend(args)
		if help {
			printNotifySendHelp(stdout)
			return 0
		}
		if err != nil {
			fmt.Fprintln(stderr, err)
			return 2
		}
		if notification.FocusBundleID == "" {
			notification.FocusBundleID = notificationFocusBundleIDFromEnvironment()
		}
		if notification.FocusTTY == "" {
			notification.FocusTTY = notificationFocusTTYFromEnvironment()
		}
		client, err := getClient()
		if err != nil {
			fmt.Fprintln(stderr, err)
			return 1
		}
		if err := client.notify(ctx, notification); err != nil {
			fmt.Fprintln(stderr, err)
			return 1
		}
	case "open", "xdg-open":
		if len(args) == 1 && (args[0] == "--help" || args[0] == "--version" || args[0] == "--manual") {
			printXDGOpenHelp(stdout)
			return 0
		}
		if len(args) != 1 {
			fmt.Fprintln(stderr, "xdg-open requires exactly one URL")
			return 2
		}
		client, err := getClient()
		if err != nil {
			fmt.Fprintln(stderr, err)
			return 1
		}
		if err := client.open(ctx, args[0]); err != nil {
			fmt.Fprintln(stderr, err)
			return 1
		}
	case "wl-copy":
		options, err := parseWlCopy(args)
		if err != nil {
			fmt.Fprintln(stderr, err)
			return 2
		}
		if options.help {
			printWlCopyHelp(stdout)
			return 0
		}
		if options.version {
			printWlClipboardVersion(stdout)
			return 0
		}
		client, err := getClient()
		if err != nil {
			fmt.Fprintln(stderr, err)
			return 1
		}
		return runWlCopy(ctx, client, options, stdin, stderr)
	case "wl-paste":
		options, err := parseWlPaste(args)
		if err != nil {
			fmt.Fprintln(stderr, err)
			return 2
		}
		if options.help {
			printWlPasteHelp(stdout)
			return 0
		}
		if options.version {
			printWlClipboardVersion(stdout)
			return 0
		}
		client, err := getClient()
		if err != nil {
			fmt.Fprintln(stderr, err)
			return 1
		}
		return runWlPaste(ctx, client, options, stdout, stderr)
	case "clipboard-write":
		if len(args) != 0 {
			fmt.Fprintln(stderr, "clipboard-write reads text from stdin and accepts no arguments")
			return 2
		}
		client, err := getClient()
		if err != nil {
			fmt.Fprintln(stderr, err)
			return 1
		}
		text, err := readBounded(stdin, maxTextClipboardBytes)
		if err == nil {
			err = client.writeClipboard(ctx, defaultTextMIME, text)
		}
		if err != nil {
			fmt.Fprintln(stderr, err)
			return 1
		}
	case "clipboard-read":
		if len(args) != 0 {
			fmt.Fprintln(stderr, "clipboard-read accepts no arguments")
			return 2
		}
		client, err := getClient()
		if err != nil {
			fmt.Fprintln(stderr, err)
			return 1
		}
		text, err := client.readClipboard(ctx, defaultTextMIME)
		if err == nil {
			_, err = stdout.Write(text)
		}
		if err != nil {
			fmt.Fprintln(stderr, err)
			return 1
		}
	case "health":
		if len(args) != 0 {
			fmt.Fprintln(stderr, "health accepts no arguments")
			return 2
		}
		client, err := getClient()
		if err != nil {
			fmt.Fprintln(stderr, err)
			return 1
		}
		if err := client.health(ctx); err != nil {
			fmt.Fprintln(stderr, err)
			return 1
		}
		fmt.Fprintln(stdout, "ok")
	default:
		fmt.Fprintf(stderr, "unsupported client command: %s\n", command)
		return 2
	}
	return 0
}

func printXDGOpenHelp(writer io.Writer) {
	_, _ = fmt.Fprintln(writer, `Usage: xdg-open URL

Open an HTTP(S) URL on the connected host.`)
}

func readBounded(reader io.Reader, limit int64) ([]byte, error) {
	contents, err := io.ReadAll(io.LimitReader(reader, limit+1))
	if err != nil {
		return nil, err
	}
	if int64(len(contents)) > limit {
		return nil, fmt.Errorf("clipboard data exceeds %d MiB limit", limit/(1024*1024))
	}
	return contents, nil
}

func printHostdUsage(writer io.Writer) {
	_, _ = fmt.Fprintln(writer, `Usage: hostd [OPTIONS]
       hostd init-token [--token-file PATH]

Run the macOS host bridge. Clipboard reads require the explicit
--allow-clipboard-read option.`)
}

func printHostctlUsage(writer io.Writer) {
	_, _ = fmt.Fprintln(writer, `Usage: hostctl COMMAND [OPTIONS]

Commands:
  notify-send       Send a host notification
  xdg-open          Open an HTTP(S) URL on the host
  wl-copy           Copy text or an image to the host clipboard
  wl-paste          Read text or an image from the host clipboard
  clipboard-write   Copy stdin to the host clipboard
  clipboard-read    Write host clipboard text to stdout
  health            Check the authenticated bridge connection`)
}
