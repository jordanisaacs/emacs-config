# emacs-config — systemd user units

| File                   | Role                                                                |
| ---------------------- | ------------------------------------------------------------------- |
| `emacs-daemon.service` | Always-on Emacs daemon (tmux/zellij-style; restarted on failure).    |

The service uses `%h` so it works for any user; it expects the
nix-built binary at `%h/.projects/emacs-daemon/emacs/result/bin/emacs`
(the path the local `pm`-managed checkout produces after
`nix build .#emacs-jd`).  Adjust `ExecStart=` / `ExecStop=` if your
build lives elsewhere.

## Install

```sh
ln -sf "$PWD"/emacs-daemon.service ~/.config/systemd/user/
systemctl --user daemon-reload
systemctl --user enable --now emacs-daemon.service
```

Verify:

```sh
systemctl --user status emacs-daemon.service
emacsclient --eval 't'
```

## Uninstall

```sh
systemctl --user disable --now emacs-daemon.service
rm ~/.config/systemd/user/emacs-daemon.service
systemctl --user daemon-reload
```
