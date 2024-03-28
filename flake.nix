{
  description = "A very basic flake";

  inputs = {
    emacs-unstable.url = "github:nix-community/emacs-overlay";

    emacs-lsp-booster.url = "github:slotThe/emacs-lsp-booster-flake";
    emacs-lsp-booster.inputs.nixpkgs.follows = "nixpkgs";

    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    lsp-snippet.url = "github:svaante/lsp-snippet";
    lsp-snippet.flake = false;

    twist.url = "github:emacs-twist/twist.nix";
    org-babel.url = "github:jordanisaacs/org-babel";

    gnu-elpa.url =
      "git+https://git.savannah.gnu.org/git/emacs/elpa.git?ref=main";
    gnu-elpa.flake = false;

    melpa.url = "github:melpa/melpa";
    melpa.flake = false;

    nongnu-elpa.url =
      "git+https://git.savannah.gnu.org/git/emacs/nongnu.git?ref=main";
    nongnu-elpa.flake = false;
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];

      flake = {
        overlays.default = inputs.nixpkgs.lib.composeManyExtensions [
          inputs.emacs-unstable.overlays.emacs
          inputs.emacs-lsp-booster.overlays.default

          inputs.twist.overlays.default
          inputs.org-babel.overlays.default

          (final: prev:
            let emacsPackage = final.emacs-pgtk;
            in {
              emacs-init = final.tangleOrgBabelFile "init.el" ./init.org {
                tangleArg = "init.el";
              };

              emacs-env = (final.emacsTwist {
                inherit emacsPackage;
                initFiles = [ final.emacs-init ];
                lockDir = ./lock;
                registries = import ./nix/registries.nix {
                  inherit inputs;
                  emacsSrc = emacsPackage.src;
                };

                inputOverrides = import ./nix/inputOverrides.nix {
                  inherit (inputs.nixpkgs) lib;
                };
              }).overrideScope (_: tprev: {
                elispPackages = tprev.elispPackages.overrideScope
                  (prev.callPackage ./nix/packageOverrides.nix {
                    inherit (tprev) emacs;
                  });
              });

              emacs-jd = prev.symlinkJoin {
                name = "emacs-jd";
                paths = [ final.emacs-env ];
                buildInputs = [ prev.makeWrapper ];
                postBuild = ''
                  wrapProgram $out/bin/emacs \
                    --prefix PATH : "${prev.emacs-lsp-booster}/bin" \
                    --set LSP_USE_PLISTS true \
                    --add-flags --init-directory="${final.emacs-config}"
                '';
              };

              emacs-config = prev.callPackage inputs.self {
                trivialBuild = final.callPackage
                  "${inputs.nixpkgs}/pkgs/build-support/emacs/trivial.nix" {
                    emacs = final.emacs-env.overrideScope (_: tprev: {
                      inherit (tprev.emacs)
                        meta nativeComp withNativeCompilation;
                    });
                  };
              };
            })
        ];
      };

      perSystem = { config, pkgs, inputs', ... }: {
        _module.args.pkgs =
          inputs'.nixpkgs.legacyPackages.extend inputs.self.overlays.default;

        packages = {
          inherit (pkgs) emacs-config emacs-env emacs-init emacs-jd;
        };

        checks = {
          build-config = config.packages.emacs-config;
          build-env = config.packages.emacs-env;
        };

        apps = pkgs.emacs-env.makeApps { lockDirName = "lock"; };

        devShells = {
          default = pkgs.mkShell {
            buildInputs =
              [ pkgs.emacs-jd pkgs.pyright pkgs.nil pkgs.fd pkgs.ripgrep ];
          };
        };
      };
    };
}
