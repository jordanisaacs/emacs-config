{ pkgs, emacsPackage }:

let
  emacsSessionEl = emacsPackage.pkgs.trivialBuild {
    pname = "emacs-session";
    version = "0.1.0";
    src = pkgs.lib.cleanSourceWith {
      src = ./.;
      filter = path: type:
        type == "directory" || builtins.elem (baseNameOf path) [
          "emacs-session-storage.el"
          "emacs-session-frames.el"
          "emacs-session.el"
        ];
    };
    turnCompilationWarningToError = true;
  };

  checks = { emacsEnv }: {
    elisp = pkgs.runCommand "emacs-session-elisp-tests" {
      nativeBuildInputs = [ emacsEnv ];
    } ''
      env HOME="$TMPDIR" emacs -Q --batch \
        -L ${emacsSessionEl}/share/emacs/site-lisp \
        -l ${./emacs-session-test.el} \
        -f ert-run-tests-batch-and-exit
      touch $out
    '';
  };
in {
  inherit emacsSessionEl checks;
}
