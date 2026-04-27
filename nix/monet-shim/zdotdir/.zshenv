# monet-shim wrapper .zshenv. The ghostel spawn advice sets
# ZDOTDIR=<this dir> (taking precedence over ghostel's own ZDOTDIR in
# `process-environment'), so this file runs first. It chains to
# ghostel's real .zshenv for its shell integration, then redirects
# ZDOTDIR at this wrapper dir so zsh reads our .zshrc (not the user's)
# next -- our .zshrc sources the user's and then prepends the monet
# shim dir to PATH so the shim wins over user-prepended bin dirs.

if [[ -n "$GHOSTEL_MONET_ZDOTDIR_CHAIN" && -r "$GHOSTEL_MONET_ZDOTDIR_CHAIN/.zshenv" ]]; then
  'builtin' 'source' '--' "$GHOSTEL_MONET_ZDOTDIR_CHAIN/.zshenv"
fi

# Ghostel's .zshenv has restored ZDOTDIR to the user's original (or
# unset it). Capture that target, then repoint ZDOTDIR at us so zsh
# reads our .zshrc.
'builtin' 'export' GHOSTEL_MONET_USER_ZDOTDIR="${ZDOTDIR-}"
'builtin' 'export' ZDOTDIR="$GHOSTEL_MONET_SHIM_ZDOTDIR"
