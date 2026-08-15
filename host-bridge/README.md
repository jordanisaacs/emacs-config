# Host bridge

The host bridge gives programs on an Arca access to a small, explicit set of
macOS desktop operations over an SSH reverse-forward.

It has two Nix outputs:

- `hostd` is the macOS daemon. It opens URLs, displays notifications, and uses
  AppKit's `NSPasteboard` API directly for text, images, and copied files.
- `hostctl` is the Linux client package. Its installed executables are
  `notify-send`, `xdg-open`, `wl-copy`, and `wl-paste`; callers never need to
  invoke a `hostctl` command.

## Authentication

Generate a token on the Mac:

```console
hostd init-token
```

This creates `~/.config/emacs-host-bridge/token` with mode `0600`. Copy the
same file to that path on the Arca, keeping it readable only by your user. Set
`EMACS_HOST_BRIDGE_TOKEN_FILE` on either side to use a different path.

The server accepts only loopback listen addresses and every request requires
the token. The client likewise refuses non-loopback bridge URLs.

## SSH forwarding

Add the reverse-forward to the SSH host entry used to reach the Arca:

```sshconfig
Host arca.ssh
  RemoteForward 24545 127.0.0.1:24545
```

The shims connect to `http://127.0.0.1:24545` on the Arca. Override that with
`EMACS_HOST_BRIDGE_URL` if the forwarded loopback port must differ.

## Running hostd

Clipboard writes are enabled by default. Clipboard reads—including image
reads—require explicit opt-in:

```console
hostd --allow-clipboard-read
```

For launchd, use the Nix store path to `hostd` as `ProgramArguments[0]` and
include `--allow-clipboard-read` only when remote clipboard reads are wanted.
A minimal user agent is:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key><string>dev.snowytrees.emacs-hostd</string>
  <key>ProgramArguments</key>
  <array>
    <string>/absolute/nix/store/path/bin/hostd</string>
    <string>--allow-clipboard-read</string>
  </array>
  <key>RunAtLoad</key><true/>
  <key>KeepAlive</key><true/>
  <key>StandardErrorPath</key><string>/tmp/emacs-hostd.log</string>
</dict>
</plist>
```

Store it at `~/Library/LaunchAgents/dev.snowytrees.emacs-hostd.plist` and load
it with `launchctl bootstrap gui/$(id -u) <plist-path>`.

## Shim behavior

```console
notify-send --app-name Emacs 'Build finished' 'Everything passed'
xdg-open https://example.com
printf 'text' | wl-copy
wl-paste --no-newline
wl-paste --list-types
wl-paste --type image/png > clipboard.png
wl-copy --type image/png < image.png
wl-paste --save
```

Text is limited to 4 MiB and must be UTF-8. Images are limited to 25 MiB.
The supported image MIME types are PNG, JPEG, TIFF, GIF, BMP, and WebP;
AppKit can synthesize PNG from other image representations on the pasteboard.
`wl-paste --save` stores a copied image, file, or folder in a private temporary
directory and prints its local path. Multiple copied Finder items produce one
path per line. File transfers are streamed, recursively preserve directories,
and are limited to 1 GiB and 100,000 entries.
Only HTTP and HTTPS URLs may be opened.
