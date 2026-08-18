{ pkgs, ... }:

# Always-on Emacs daemon, tmux/zellij-style.
#
# ExecStart/ExecStop pin the nix store path of `pkgs.emacs-jd' directly:
# systemd's user manager uses its own (login-time) PATH for ExecStart name
# resolution, NOT the unit's Environment=PATH=, so a bare `emacs' would
# resolve to whatever is first on the manager's PATH (often the system
# AppImage at /usr/local/bin/emacs) rather than the home-manager-installed
# build.  Pinning the store path bypasses this.
#
# The Environment= PATH still matters: it's what the running Emacs sees, so
# `executable-find' / `process-file' (e.g. the pm package shelling out to
# `pm') resolve correctly without absolute paths in defcustoms.
{
  systemd.user.services.emacs-daemon = {
    Unit = {
      Description = "Emacs daemon (always-on, tmux/zellij-style)";
      Documentation = "info:emacs man:emacs(1) https://www.gnu.org/software/emacs/";
    };
    Service = {
      Type = "notify";
      Environment = "PATH=%h/bin:%h/.local/bin:%h/.nix-profile/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin";
      ExecStart = "${pkgs.emacs-jd}/bin/emacs --fg-daemon";
      ExecStop = ''${pkgs.emacs-jd}/bin/emacsclient --eval "(kill-emacs)"'';
      Restart = "on-failure";
      TimeoutStartSec = 90;
      TimeoutStopSec = 30;
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
  };
}
