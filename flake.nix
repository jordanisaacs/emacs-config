{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    emacs-unstable.url = "github:nix-community/emacs-overlay";

    emacs-lsp-booster.url = "github:slotThe/emacs-lsp-booster-flake";
    emacs-lsp-booster.inputs.nixpkgs.follows = "nixpkgs";

    systems.url = "github:nix-systems/default";

    twist.url = "github:emacs-twist/twist.nix";
    org-babel.url = "github:emacs-twist/org-babel";
    twist-overrides.url = "github:emacs-twist/overrides";

    gnu-elpa.url = "github:elpa-mirrors/elpa";
      # "git+https://git.savannah.gnu.org/git/emacs/elpa.git?ref=main";
    gnu-elpa.flake = false;

    melpa.url = "github:melpa/melpa";
    melpa.flake = false;

    nongnu-elpa.url = "github:elpa-mirrors/nongnu";
      # "git+https://git.savannah.gnu.org/git/emacs/nongnu.git?ref=main";
    nongnu-elpa.flake = false;

    zig2nix.url = "github:Cloudef/zig2nix";
    zig2nix.inputs.nixpkgs.follows = "nixpkgs";

    ghostel.url = "git+file:./submodules/ghostel";
    ghostel.flake = false;

    # Source for `pm' (project_manager).  Pinned to remote master.
    pm-src.url = "github:jordan-isaacs_data/project-manager";
    pm-src.flake = false;
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;

      flake = {
        homeManagerModules.emacs-daemon =
          ./integrations/home-manager/emacs-daemon.nix;
        homeManagerModules.ghostel-editor =
          ./integrations/home-manager/ghostel-editor.nix;
      };

      perSystem = { config, pkgs, lib, system, ... }:
        let
          overlays = [
            inputs.emacs-unstable.overlays.emacs
            inputs.emacs-lsp-booster.overlays.default
          ];

          emacsInit = pkgs.writeText "init.el"
            (inputs.org-babel.lib.tangleOrgBabel { tangleArg = "init.el"; }
              (builtins.readFile ./init.org));

          emacsPackage = pkgs.emacs-git-pgtk.overrideAttrs (old: {
            patches = (old.patches or [ ])
              ++ [
                ./nix/patches/eln-cache-correct-spot.patch
                ./nix/patches/tty-synchronized-output.patch
              ];
          });

          fwatcher = pkgs.rustPlatform.buildRustPackage {
            pname = "fwatcher";
            version = "0.1.0";
            src = ./nix/eglot-fwatcher;
            cargoLock.lockFile = ./nix/eglot-fwatcher/Cargo.lock;
            meta.mainProgram = "fwatcher";
          };

          eglotFwatcherEl = emacsPackage.pkgs.trivialBuild {
            pname = "eglot-fwatcher";
            version = "0.1.0";
            src = ./nix/eglot-fwatcher/elisp;
            preBuild = ''
              substituteInPlace eglot-fwatcher.el \
                --replace-fail '"fwatcher"' '"${fwatcher}/bin/fwatcher"'
            '';
          };

          monetShim = pkgs.runCommand "monet-shim" { } ''
            mkdir -p $out/bin $out/zdotdir
            substitute ${./nix/monet-shim/claude} $out/bin/claude \
              --replace-fail '@EMACSCLIENT@' '${emacsPackage}/bin/emacsclient'
            chmod +x $out/bin/claude
            install -m0644 ${./nix/monet-shim/zdotdir/.zshenv} $out/zdotdir/.zshenv
            install -m0644 ${./nix/monet-shim/zdotdir/.zshrc} $out/zdotdir/.zshrc
          '';

          ghostelEditor = pkgs.runCommand "ghostel-editor" { } ''
            mkdir -p $out/bin
            substitute ${./nix/ghostel-editor/ghostel-editor} $out/bin/ghostel-editor \
              --replace-fail '@EMACSCLIENT@' '${emacsPackage}/bin/emacsclient'
            chmod +x $out/bin/ghostel-editor
          '';

          ghostelModule = let
            zigEnv = inputs.zig2nix.outputs.zig-env.${system} {
              zig = inputs.zig2nix.outputs.packages.${system}.zig-0_15_2;
            };
          in zigEnv.package {
            pname = "ghostel-module";
            version = "0.18.1";
            src = inputs.ghostel;
            # build.zig.zon2json-lock is regenerated via
            #   nix run github:Cloudef/zig2nix#zon2json-lock -- build.zig.zon
            # inside a checkout of the ghostel flake input.
            zigBuildZonLock = ./nix/ghostel/build.zig.zon2json-lock;
            zigBuildFlags = [ "-Doptimize=ReleaseFast" "-Dcpu=baseline" ];
            preBuild = ''
              # ghostel's build.zig installs the module one level above the
              # zig install prefix to make it easy to dlopen from the repo;
              # under Nix that path escapes $out, so flatten it.
              substituteInPlace build.zig \
                --replace-fail '"../ghostel-module.so"' '"ghostel-module.so"' \
                --replace-fail '"../ghostel-module.dylib"' '"ghostel-module.dylib"'
            '';
            postInstall = ''
              rm -f $out/lib/libghostel-module.so $out/lib/libghostel-module.dylib
              rmdir $out/lib 2>/dev/null || true
            '';
          };

          twistArgs = {
            inherit pkgs emacsPackage;

            nativeCompileAheadDefault = true;
            lockDir = ./lock;
            initFiles = [ emacsInit ];
            initParser = inputs.twist.lib.parseUsePackages {
              inherit (inputs.nixpkgs) lib;
            } { };

            registries = (import ./nix/registries.nix {
              inherit inputs;
              emacsSrc = emacsPackage.src;
            });

            inputOverrides =
              import ./nix/inputOverrides.nix { inherit (inputs.nixpkgs) lib; };

            extraSiteStartElisp = let
              treesitterPackage =
                emacsPackage.pkgs.treesit-grammars.with-all-grammars;
            in ''
              (when init-file-user
                (add-to-list 'treesit-extra-load-path "${treesitterPackage}/lib"))
              (add-to-list 'load-path "${eglotFwatcherEl}/share/emacs/site-lisp")
              (defvar my/monet-shim-dir "${monetShim}/bin"
                "Directory holding the nix-provided `claude' PATH shim.")
              (defvar my/monet-shim-zdotdir "${monetShim}/zdotdir"
                "ZDOTDIR wrapper that chains ghostel's .zshenv and then our .zshrc; ensures the monet shim stays first in PATH after user's .zshrc runs.")
            '';
          };

          emacsEnv = (inputs.twist.lib.makeEnv twistArgs).overrideScope
            (lib.composeExtensions inputs.twist-overrides.overlays.twistScope
              (_: tsuper: {
                elispPackages = tsuper.elispPackages.overrideScope
                  (import ./nix/packageOverrides.nix {
                    inherit pkgs ghostelModule;
                    ghostelSrc = inputs.ghostel;
                    pmSrc = inputs.pm-src;
                  });
              }));

          emacsConfig = pkgs.callPackage inputs.self {
            trivialBuild = pkgs.callPackage
              "${inputs.nixpkgs}/pkgs/applications/editors/emacs/build-support/trivial.nix" {
                emacs = emacsEnv.overrideScope (_: tprev: {
                  inherit (tprev.emacs) meta nativeComp withNativeCompilation;
                });
              };
            emacsFuncs =
              "${inputs.nixpkgs}/pkgs/applications/editors/emacs/setup-hook.sh";
          };

          emacs-jd = pkgs.symlinkJoin {
            name = "emacs-jd";
            paths = [ emacsEnv ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/emacs \
                --prefix PATH : "${
                  lib.makeBinPath [ pkgs.emacs-lsp-booster pkgs.nodejs pkgs.perl pkgs.fd pkgs.ripgrep pkgs.delta fwatcher ]
                }" \
               --set LSP_USE_PLISTS true \
               --set DICPATH "${pkgs.hunspellDicts.en_US}/share/hunspell" \
               --add-flags --init-directory="${emacsConfig}"
            '';
            meta.mainProgram = "emacs";
          };
        in {
          _module.args.pkgs =
            import inputs.nixpkgs { inherit system overlays; };

          packages = {
            inherit emacsConfig emacs-jd emacsEnv emacsInit emacsPackage
              ghostelModule fwatcher eglotFwatcherEl monetShim
              ghostelEditor;
            default = emacs-jd;
          };

          checks = {
            # Check if the elisp packages are successfully built.
            build-config = emacsConfig;
            build-env =
              emacsEnv.overrideScope (_: _: { executablePackages = [ ]; });
          };

          apps = emacsEnv.makeApps { lockDirName = "lock"; };

          devShells = {
            default = pkgs.mkShell {
              buildInputs = [
                emacs-jd
                pkgs.pyright
                pkgs.python312Packages.pytest
                pkgs.nil
                pkgs.fd
                pkgs.ripgrep
                pkgs.clang-tools

                pkgs.go
                pkgs.gopls

                pkgs.rustc
                pkgs.cargo
                pkgs.rustfmt
                pkgs.clippy
                pkgs.rust-analyzer
              ];
            };
          };
        };
    };
}
