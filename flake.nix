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

    gnu-elpa.url =
      "git+https://git.savannah.gnu.org/git/emacs/elpa.git?ref=main";
    gnu-elpa.flake = false;

    melpa.url = "github:melpa/melpa";
    melpa.flake = false;

    nongnu-elpa.url =
      "git+https://git.savannah.gnu.org/git/emacs/nongnu.git?ref=main";
    nongnu-elpa.flake = false;
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;

      perSystem = { config, pkgs, lib, system, ... }:
        let
          overlays = [
            inputs.emacs-unstable.overlays.emacs
            inputs.emacs-lsp-booster.overlays.default
          ];

          emacsInit = pkgs.writeText "init.el"
            (inputs.org-babel.lib.tangleOrgBabel { tangleArg = "init.el"; }
              (builtins.readFile ./init.org));

          emacsPackage = pkgs.emacs-pgtk;

          twistArgs = {
            inherit pkgs emacsPackage;

            nativeCompileAheadDefault = true;
            lockDir = ./lock;
            initFiles = [ emacsInit ];
            initParser =
              inputs.twist.lib.parseUsePackages { inherit (inputs.nixpkgs) lib; } { };

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
            '';
          };

          emacsEnv = (inputs.twist.lib.makeEnv twistArgs).overrideScope
            (lib.composeExtensions inputs.twist-overrides.overlays.twistScope
              (_: tsuper: {
                elispPackages = tsuper.elispPackages.overrideScope
                  (import ./nix/packageOverrides.nix { inherit pkgs; });
              }));

          emacsConfig = pkgs.callPackage inputs.self {
            trivialBuild = pkgs.callPackage
              "${inputs.nixpkgs}/pkgs/applications/editors/emacs/build-support/trivial.nix" {
                emacs = emacsEnv.overrideScope (_: tprev: {
                  inherit (tprev.emacs) meta nativeComp withNativeCompilation;
                });
              };
	    emacsFuncs = "${inputs.nixpkgs}/pkgs/applications/editors/emacs/build-support/emacs-funcs.sh";
          };

          emacs-jd = pkgs.symlinkJoin {
            name = "emacs-jd";
            paths = [ emacsEnv ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/emacs \
                --prefix PATH : "${pkgs.emacs-lsp-booster}/bin" \
                --set LSP_USE_PLISTS true \
                --add-flags --init-directory="${emacsConfig}"
            '';
          };
        in {
          _module.args.pkgs =
            import inputs.nixpkgs { inherit system overlays; };

          packages = {
            inherit emacsConfig emacs-jd emacsEnv emacsInit emacsPackage;
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
              ];
            };
          };
        };
    };
}
