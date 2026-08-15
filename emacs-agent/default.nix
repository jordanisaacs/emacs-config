{ pkgs, emacsPackage }:

let
  emacsAgentEl = pkgs.runCommand "emacs-agent-elisp" { } ''
    site=$out/share/emacs/site-lisp
    libexec=$out/libexec
    mkdir -p "$site" "$libexec"
    install -m0555 ${./emacs_agent_native_title.py} \
      "$libexec/emacs-agent-native-title"
    substituteInPlace "$libexec/emacs-agent-native-title" \
      --replace-fail '@PYTHON@' '${pkgs.python3}/bin/python3'
    install -m0444 ${./emacs-agent-rules.el} \
      "$site/emacs-agent-rules.el"
    substitute ${./emacs-agent-track.el} \
      "$site/emacs-agent-track.el" \
      --replace-fail '@EMACS_AGENT_NATIVE_TITLE@' \
        "$libexec/emacs-agent-native-title"
    install -m0444 ${./emacs-agent-sidebar.el} \
      "$site/emacs-agent-sidebar.el"
    install -m0444 ${./emacs-agent.el} \
      "$site/emacs-agent.el"
  '';

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
      env HOME="$TMPDIR" emacs -Q --batch \
        -L ${emacsAgentEl}/share/emacs/site-lisp \
        -l ${./emacs-agent-test.el} \
        -f ert-run-tests-batch-and-exit
      touch $out
    '';
  };
in {
  inherit emacsAgentEl emacsAgentCli checks;
}
