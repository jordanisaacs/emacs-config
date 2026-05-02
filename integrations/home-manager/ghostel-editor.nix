{ pkgs, ... }:

# Wire $VISUAL through the `ghostelEditor' wrapper. Inside a
# ghostel-managed terminal the file opens in the running Emacs daemon
# via emacsclient; otherwise the wrapper falls back to $EDITOR.
#
# We set $VISUAL (which most tools, including Claude Code, prefer over
# $EDITOR) so the user's $EDITOR remains untouched and naturally serves
# as the fallback inside the wrapper.
{
  home.packages = [ pkgs.ghostelEditor ];
  home.sessionVariables = {
    VISUAL = "${pkgs.ghostelEditor}/bin/ghostel-editor";
  };
}
