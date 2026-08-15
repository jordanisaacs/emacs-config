{ pkgs, system, upstreamSrc, zig2nix, emacsPackage }:

let
  ghostelEditor = pkgs.runCommand "ghostel-editor" { } ''
    mkdir -p $out/bin
    substitute ${./ghostel-editor} $out/bin/ghostel-editor \
      --replace-fail '@EMACSCLIENT@' '${emacsPackage}/bin/emacsclient'
    chmod +x $out/bin/ghostel-editor
  '';

  ghostelSrc = pkgs.applyPatches {
    name = "ghostel-source-0.50.0";
    src = upstreamSrc;
    patches = [ ./patches/agent-monitor.patch ];
  };

  ghostelModule = let
    zig = zig2nix.outputs.packages.${system}.zig-0_16_0;
    zigEnv = zig2nix.outputs.zig-env.${system} { inherit zig; };
  in zigEnv.package {
    pname = "ghostel-module";
    version = "0.50.0";
    src = ghostelSrc;
    # Regenerate with:
    #   nix run github:Cloudef/zig2nix#zon2json-lock -- build.zig.zon
    # from a checkout of the pinned Ghostel source.
    zigBuildZonLock = ./build.zig.zon2json-lock;
    zigBuildFlags = [ "-Doptimize=ReleaseFast" "-Dcpu=baseline" ];
    # zig2nix's Linux hook wraps Zig in bubblewrap to provide /usr/bin/env.
    # Ghostty's nested `zig env' cannot run in nested bubblewrap, so use the
    # underlying static Zig binary for both invocations.
    preBuild = ''
      export PATH=${zig}/bin:$PATH
    '';
  };

  elispPackageOverride = _final: prev: {
    # Install the Nix-built native module beside ghostel.el. Runtime module
    # downloads cannot write into the Nix store.
    ghostel = prev.ghostel.overrideAttrs (old:
      let moduleSuffix = pkgs.stdenv.hostPlatform.extensions.sharedLibrary;
      in {
        src = ghostelSrc;
        preBuild = (old.preBuild or "") + ''
          install -m444 ${ghostelModule}/ghostel-module${moduleSuffix} \
            ghostel-module${moduleSuffix}
          install -m444 ${ghostelModule}/ghostel-module.version \
            ghostel-module.version
        '';
      });
  };
in {
  inherit ghostelEditor ghostelSrc ghostelModule elispPackageOverride;
}
