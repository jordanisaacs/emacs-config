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

    gnu-elpa.url = "git+https://git.savannah.gnu.org/git/emacs/elpa.git?ref=main";
    gnu-elpa.flake = false;

    melpa.url = "github:melpa/melpa";
    melpa.flake = false;

    nongnu-elpa.url = "git+https://git.savannah.gnu.org/git/emacs/nongnu.git?ref=main";
    nongnu-elpa.flake = false;
  };


  outputs = inputs @ { flake-parts, ... }:
  flake-parts.lib.mkFlake {inherit inputs;} {
    systems = ["x86_64-linux"];

    flake = {
      overlays.default = inputs.nixpkgs.lib.composeManyExtensions [
        inputs.emacs-unstable.overlays.emacs
        inputs.emacs-lsp-booster.overlays.default
        
        inputs.twist.overlays.default
        inputs.org-babel.overlays.default

        (final: prev: let
          emacsPackage = final.emacs-pgtk;

      # emacs-boost = pkgs.symlinkJoin {
      #   name = "emacs-booster";
      #   paths = [ my-emacs ];
      #   buildInputs = [pkgs.makeWrapper];
      #   postBuild = ''
      #     wrapProgram $out/bin/emacs \
      #       --prefix PATH : "${pkgs.emacs-lsp-booster}/bin" \
      #       --set LSP_USE_PLISTS true
      #   '';
      # };
        in {
          emacs-init = final.tangleOrgBabelFile "init.el" ./init.org {tangleArg = "init.el";};

          emacs-env = (final.emacsTwist {
            inherit emacsPackage;
            initFiles = [final.emacs-init];
            lockDir = ./lock;
            registries = import ./nix/registries.nix {
                inherit inputs;
                emacsSrc = emacsPackage.src;
            };

            inputOverrides = import ./nix/inputOverrides.nix {inherit (inputs.nixpkgs) lib;};
          }).overrideScope (_: tprev: {
            elispPackages = tprev.elispPackages.overrideScope (
              prev.callPackage ./nix/packageOverrides.nix {inherit (tprev) emacs;}
            );
          });


          emacs-boost = prev.symlinkJoin {
             name = "emacs-booster";
             paths = [final.emacs-env];
             buildInputs = [prev.makeWrapper];
             postBuild = ''
               wrapProgram $out/bin/emacs \
                 --prefix PATH : "${prev.emacs-lsp-booster}/bin"
             '';
          };

          emacs-config = prev.callPackage inputs.self {
            trivialBuild = final.callPackage "${inputs.nixpkgs}/pkgs/build-support/emacs/trivial.nix" {
               emacs = final.emacs-env.overrideScope (_: tprev: {
                 inherit (tprev.emacs) meta nativeComp withNativeCompilation;
               });
            };
          };
        })
      ];
    };

    perSystem = {
      config,
      pkgs,
      inputs',
      ...
    }: {
        _module.args.pkgs = inputs'.nixpkgs.legacyPackages.extend inputs.self.overlays.default;

        packages = {
          inherit (pkgs) emacs-config emacs-env emacs-init emacs-boost;
        };

        checks = {
          build-config = config.packages.emacs-config;
          build-env = config.packages.emacs-env;
        };

        apps = pkgs.emacs-env.makeApps {
          lockDirName = "lock";
        };

        devShells = {
        default = pkgs.mkShell {
        buildInputs = [pkgs.emacs-env];
        };
        };
    };
  };
}
      #jinherit (inp
      #my-emacs = (pkgs.emacsWithPackagesFromUsePackage {
      #  package = pkgs.emacs-gtk;
        # pgtk fails to set the 'CLIPBOARD selection for a region
        # but it works in literally all other circumstances :/
        #   pkgs.emacs-pgtk.overrideAttrs (old: {
        #   configureFlags = old.configureFlags ++ ["--with-json=no"];
        # });
        #config = ./init.el;
        #defaultInitFile = true;
        #alwaysEnsure = false;
        #alwaysTangle = false;
        # override = final: prev: {
        #   lsp-mode = prev.melpaPackages.lsp-mode.overrideAttrs(old: {
        #     LSP_USE_PLISTS = true;
        #   });
        #   ccls = prev.melpaPackages.ccls.overrideAttrs(old: {
        #     LSP_USE_PLISTS = true;
        #   });
        # };
        # extraEmacsPackages = epkgs: [
        #   (epkgs.treesit-grammars.with-grammars (grammars:
        #     with grammars; [
        #       tree-sitter-bash
        #       tree-sitter-python
        #       tree-sitter-elisp
        #       tree-sitter-nix
        #       tree-sitter-rust
        #       tree-sitter-cmake
        #       tree-sitter-toml
        #       tree-sitter-make
        #       tree-sitter-org-nvim
        #       tree-sitter-markdown
        #       tree-sitter-zig
        #     ])) (epkgs.trivialBuild rec {
        #       pname = "lsp-snippet";
        #       version = "main-1-22-24";
        #       src = lsp-snippet;
        #       preBuild = ''
        #       rm lsp-snippet-yasnippet.el
        #     '';
        #       propagatedUserEnvPkgs = [
        #         epkgs.tempel
        #       ];
        #       buildInputs = propagatedUserEnvPkgs;
        #     })
      #  ];
      #});

