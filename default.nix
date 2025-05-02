{ lib, stdenv, trivialBuild, emacsFuncs, xorg }:
let
  init = (trivialBuild {
    pname = "config-init";
    version = "1";
    src = lib.sourceByRegex ./. [ "init.org" ];
    turnCompilationWarningToError = true;
    preBuild = ''
      emacs --batch --quick \
          --load org \
          *.org \
          -funcall org-babel-tangle
    '';
    fixupPhase = ''
      if [ -d "$HOME/.emacs.d/eln-cache" ]; then
          mv $HOME/.emacs.d/eln-cache/* $out/share/emacs/native-lisp
      fi
    '';

    setupHook = emacsFuncs;
  }).overrideAttrs (_: {
    postInstall = ''
      # Besides adding the output directory to the native load path, make sure
      # the current package's elisp files are in the load path, otherwise
      # (require 'file-b) from file-a.el in the same package will fail.
      mkdir -p $out/share/emacs/native-lisp

      # package-activate-all is used to activate packages.  In other builder
      # helpers, package-initialize is used for this purpose because
      # package-activate-all is not available before Emacs 27.
      export HOME="$(mktemp -d)"
      find $out/share/emacs -type f -name '*.el' -not -name ".dir-locals.el" -print0 \
        | xargs --verbose -0 -I {} -n 1 -P $NIX_BUILD_CORES sh -c \
            "emacs \
               --batch \
               -f package-activate-all \
               --eval '(let ((default-directory \"$out/share/emacs/site-lisp\")) (normal-top-level-add-subdirs-to-load-path))' \
               --eval '(setq large-file-warning-threshold nil)' \
               --eval '(setq byte-compile-error-on-warn t)' \
               -f batch-native-compile {}"
    '';
  });
in stdenv.mkDerivation {
  name = "emacs-config";
  src = ./.;
  dontUnpack = true;
  buildInputs = [ ];

  passthru.components = { inherit init; };

  installPhase = ''
    mkdir -p $out
    ${xorg.lndir}/bin/lndir -silent ${init}/share/emacs/site-lisp $out

    if [ -d "${init}/share/emacs/native-lisp" ]; then
      mkdir -p $out/eln-cache
      ${xorg.lndir}/bin/lndir -silent ${init}/share/emacs/native-lisp $out/eln-cache
    fi
  '';
}
