{ pkgs, emacsPackage }:

let
  emacsAgentEl = emacsPackage.pkgs.trivialBuild {
    pname = "emacs-agent";
    version = "0.1.0";
    src = pkgs.lib.cleanSourceWith {
      src = ./.;
      filter = path: type:
        type == "directory" || builtins.elem (baseNameOf path) [
          "emacs-agent-rules.el"
          "emacs-agent-track.el"
          "emacs-agent-bookmark.el"
          "emacs-agent.el"
        ];
    };
    preBuild = ''
      substituteInPlace emacs-agent-track.el \
        --replace-fail '@EMACS_AGENT_NATIVE_TITLE@' \
          "$out/libexec/emacs-agent-native-title"
    '';
    # The sidebar is installed after native compilation below: it depends on
    # the PM package built inside the final Twist environment.  The check
    # byte-compiles the complete installed package in that environment.
    buildPhase = ''
      runHook preBuild
      emacs -l package -f package-initialize \
        --eval '(setq byte-compile-debug t)' \
        --eval '(setq byte-compile-error-on-warn t)' \
        -L . --batch -f batch-byte-compile \
        emacs-agent-rules.el emacs-agent-track.el \
        emacs-agent-bookmark.el emacs-agent.el
      runHook postBuild
    '';
    postInstall = ''
      install -m0444 ${./emacs-agent-sidebar.el} \
        "$out/share/emacs/site-lisp/emacs-agent-sidebar.el"
      mkdir -p "$out/libexec"
      substitute ${./emacs_agent_native_title.py} \
        "$out/libexec/emacs-agent-native-title" \
        --replace-fail '@PYTHON@' '${pkgs.python3}/bin/python3'
      chmod 0555 "$out/libexec/emacs-agent-native-title"
    '';
    turnCompilationWarningToError = true;
  };

  emacsAgentCli = pkgs.runCommand "emacs-agent-cli" { } ''
    mkdir -p $out/bin
    install -m0555 ${./emacs_agent.py} $out/bin/emacs-agent
    substituteInPlace $out/bin/emacs-agent \
      --replace-fail '@PYTHON@' '${pkgs.python3}/bin/python3' \
      --replace-fail '@EMACSCLIENT@' '${emacsPackage}/bin/emacsclient'
  '';

  checks = { emacsEnv }: {
    cli = emacsAgentCli;

    python = pkgs.runCommand "emacs-agent-python-tests" {
      nativeBuildInputs = [ pkgs.python3 ];
    } ''
      export PYTHONDONTWRITEBYTECODE=1
      python3 -m unittest discover -v -s ${./.}
      touch $out
    '';

    elisp = pkgs.runCommand "emacs-agent-elisp-tests" {
      nativeBuildInputs = [ emacsEnv ];
    } ''
      compileDir="$TMPDIR/emacs-agent-compile"
      mkdir -p "$compileDir"
      cp ${emacsAgentEl}/share/emacs/site-lisp/*.el "$compileDir/"
      emacs -Q --batch -L "$compileDir" \
        --eval '(setq byte-compile-error-on-warn t)' \
        -f batch-byte-compile "$compileDir"/*.el
      env HOME="$TMPDIR" emacs -Q --batch \
        -L ${emacsAgentEl}/share/emacs/site-lisp \
        -l ${./emacs-agent-test.el} \
        -l ${./emacs-agent-bookmark-test.el} \
        -f ert-run-tests-batch-and-exit
      touch $out
    '';
  };
in {
  inherit emacsAgentEl emacsAgentCli checks;
}
