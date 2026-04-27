# monet-shim wrapper .zshrc. Restores ZDOTDIR to the user's target so
# their .zshrc and any later startup files see the expected path, then
# sources the user's .zshrc. After the user's config has finished
# rearranging PATH, we prepend the monet shim dir so `claude' resolves
# there first -- even when .zshrc front-loads `$HOME/.local/bin' etc.

if [[ -n "$GHOSTEL_MONET_USER_ZDOTDIR" ]]; then
  'builtin' 'export' ZDOTDIR="$GHOSTEL_MONET_USER_ZDOTDIR"
else
  'builtin' 'unset' 'ZDOTDIR'
fi
'builtin' 'unset' 'GHOSTEL_MONET_USER_ZDOTDIR' \
                  'GHOSTEL_MONET_SHIM_ZDOTDIR' \
                  'GHOSTEL_MONET_ZDOTDIR_CHAIN'

'builtin' 'typeset' _monet_user_zshrc="${ZDOTDIR-$HOME}/.zshrc"
if [[ -r "$_monet_user_zshrc" ]]; then
  'builtin' 'source' '--' "$_monet_user_zshrc"
fi
'builtin' 'unset' '_monet_user_zshrc'

if [[ -n "$GHOSTEL_MONET_SHIM" ]]; then
  case ":$PATH:" in
    ":$GHOSTEL_MONET_SHIM:"*) ;;
    *)
      PATH=${PATH//":$GHOSTEL_MONET_SHIM:"/":"}
      PATH="${GHOSTEL_MONET_SHIM}:${PATH}"
      'builtin' 'export' PATH
      ;;
  esac
fi
