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

    # Upstream ghostel; pin tracked by flake.lock. To bump:
    #   1. `nix flake update ghostel`,
    #   2. bump `ghostelModule.version` to match upstream `build.zig.zon`,
    #   3. regenerate `ghostel/build.zig.zon2json-lock` if upstream
    #      touched `build.zig.zon` (see `ghostel/default.nix`).
    ghostel.url = "github:dakra/ghostel";
    ghostel.flake = false;

    # Source for `pm' (project_manager).  Pinned to remote master.
    pm-src.url = "github:jordanisaacs/project-manager";
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

          emacsPackage = import ./emacs { inherit pkgs; };

          eglotFwatcherBuild =
            import ./eglot-fwatcher { inherit pkgs emacsPackage; };
          inherit (eglotFwatcherBuild) fwatcher eglotFwatcherEl;

          emacsAgentBuild =
            import ./emacs-agent { inherit pkgs emacsPackage; };
          inherit (emacsAgentBuild) emacsAgentEl emacsAgentCli;

          ghostelBuild = import ./ghostel {
            inherit pkgs system emacsPackage;
            upstreamSrc = inputs.ghostel;
            zig2nix = inputs.zig2nix;
          };
          inherit (ghostelBuild) ghostelEditor ghostelSrc ghostelModule;

          hostd = pkgs.buildGoModule {
            pname = "hostd";
            version = "0.1.0";
            src = ./host-bridge;
            vendorHash = null;
            subPackages = [ "cmd/hostd" ];
            env.CGO_ENABLED = "1";
            postPatch = ''
              substituteInPlace host_darwin.go \
                --replace-fail '"terminal-notifier"' \
                '"${lib.getExe pkgs.terminal-notifier}"'
            '';
            meta = {
              description = "Authenticated macOS host integration daemon";
              mainProgram = "hostd";
              platforms = lib.platforms.darwin;
            };
          };

          hostctl = pkgs.buildGoModule {
            pname = "hostctl";
            version = "0.1.0";
            src = ./host-bridge;
            vendorHash = null;
            subPackages = [ "cmd/hostctl" ];
            postInstall = ''
              hostctlBinary=$out/bin/hostctl
              for shim in notify-send xdg-open wl-copy wl-paste; do
                install -m0755 "$hostctlBinary" "$out/bin/$shim"
              done
              rm "$hostctlBinary"
            '';
            meta = {
              description = "Linux command shims for the macOS host integration daemon";
              platforms = lib.platforms.linux;
            };
          };

          monetShim = pkgs.runCommand "monet-shim" { } ''
            mkdir -p $out/bin $out/zdotdir
            substitute ${./nix/monet-shim/claude} $out/bin/claude \
              --replace-fail '@EMACSCLIENT@' '${emacsPackage}/bin/emacsclient'
            chmod +x $out/bin/claude
            install -m0644 ${./nix/monet-shim/zdotdir/.zshenv} $out/zdotdir/.zshenv
            install -m0644 ${./nix/monet-shim/zdotdir/.zshrc} $out/zdotdir/.zshrc
          '';

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
              ;; Emacs owns live agent tracking and control; pm only supplies
              ;; project resolution and launch/history operations.
              (add-to-list 'load-path "${emacsAgentEl}/share/emacs/site-lisp")
              (autoload 'emacs-agent-api-call-base64 "emacs-agent")
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
                  (lib.composeExtensions
                    (import ./nix/packageOverrides.nix {
                      inherit pkgs;
                      pmSrc = inputs.pm-src;
                    })
                    ghostelBuild.elispPackageOverride);
              }));

          emacsAgentChecks = emacsAgentBuild.checks { inherit emacsEnv; };

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
            paths = [ emacsEnv emacsAgentCli ];
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
              ghostelModule fwatcher eglotFwatcherEl emacsAgentEl emacsAgentCli monetShim
              ghostelEditor hostd hostctl;
            default = emacs-jd;
          };

          checks = {
            # Check if the elisp packages are successfully built.
            build-config = emacsConfig;
            build-env =
              emacsEnv.overrideScope (_: _: { executablePackages = [ ]; });
            emacs-agent-cli = emacsAgentChecks.cli;
            emacs-agent-python-tests = emacsAgentChecks.python;
            emacs-agent-elisp-tests = emacsAgentChecks.elisp;
            patched-ghostel = ghostelModule;
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
