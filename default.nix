{
    lib,
    stdenv,
    trivialBuild,
    xorg
}: let
    init = trivialBuild {
        pname = "config-init";
        version ="1";
        src = lib.sourceByRegex ./. ["init.org"];
        preBuild = ''
        emacs --batch --quick \
            --load org \
            *.org \
            -funcall org-babel-tangle
        '';

        buildPhase = ''
        runHook preBuild

        export HOME="$(mktemp -d)"
        emacs -L . --batch --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile *.el

        runHook postBuild
        '';

        fixupPhase = ''
        if [ -d "$HOME/.emacs.d/eln-cache" ]; then
          mv $HOME/.emacs.d/eln-cache/* $out/share/emacs/native-lisp
        fi
        '';
    };
in
stdenv.mkDerivation {
name = "emacs-config";
src = ./.;
dontUnpack = true;
buildInputs = [];

passthru.components = {
inherit init;
};

installPhase = ''
  mkdir -p $out
  ${xorg.lndir}/bin/lndir -silent ${init}/share/emacs/site-lisp $out

  if [ -d "${init}/share/emacs/native-lisp" ]; then
    mkdir -p $out/eln-cache
    ${xorg.lndir}/bin/lndir -silent ${init}/share/emacs/native-lisp $out/eln-cache
  fi
'';
}
