{ pkgs, emacsPackage }:

let
  fwatcher = pkgs.rustPlatform.buildRustPackage {
    pname = "fwatcher";
    version = "0.1.0";
    src = ./.;
    cargoLock.lockFile = ./Cargo.lock;
    meta.mainProgram = "fwatcher";
  };

  eglotFwatcherEl = emacsPackage.pkgs.trivialBuild {
    pname = "eglot-fwatcher";
    version = "0.1.0";
    src = ./elisp;
    preBuild = ''
      substituteInPlace eglot-fwatcher.el \
        --replace-fail '"fwatcher"' '"${fwatcher}/bin/fwatcher"'
    '';
  };
in {
  inherit fwatcher eglotFwatcherEl;
}
