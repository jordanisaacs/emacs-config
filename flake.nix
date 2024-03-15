{
  description = "A very basic flake";

  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    emacs-lsp-booster.url = "github:slotThe/emacs-lsp-booster-flake";
    emacs-lsp-booster.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    lsp-snippet.url = "github:svaante/lsp-snippet";
    lsp-snippet.flake = false;
  };


  outputs = { self, nixpkgs, emacs-overlay, emacs-lsp-booster, lsp-snippet }:
  let
      pkgs = import nixpkgs {
      overlays = [(import emacs-overlay) emacs-lsp-booster.overlays.default];
      system = "x86_64-linux";
      };

      my-emacs = (pkgs.emacsWithPackagesFromUsePackage {
        package = pkgs.emacs-gtk;
        #   pkgs.emacs-pgtk.overrideAttrs (old: {
        #   configureFlags = old.configureFlags ++ ["--with-json=no"];
        # });
        config = ./init.el;
        defaultInitFile = true;
        alwaysEnsure = false;
        alwaysTangle = false;
        override = final: prev: {
          lsp-mode = prev.melpaPackages.lsp-mode.overrideAttrs(old: {
             LSP_USE_PLISTS = true;
          });
          ccls = prev.melpaPackages.ccls.overrideAttrs(old: {
            LSP_USE_PLISTS = true;
          });
        };
        extraEmacsPackages = epkgs: [ (epkgs.treesit-grammars.with-grammars (grammars:
        with grammars; [
            tree-sitter-bash
            tree-sitter-python
            tree-sitter-elisp
            tree-sitter-nix
            tree-sitter-rust
            tree-sitter-cmake
            tree-sitter-toml
            tree-sitter-make
            tree-sitter-org-nvim
            tree-sitter-markdown
            tree-sitter-zig
        ])) (epkgs.trivialBuild rec {
          pname = "lsp-snippet";
          version = "main-1-22-24";
          src = lsp-snippet;
          preBuild = ''
             rm lsp-snippet-yasnippet.el
          '';
          propagatedUserEnvPkgs = [
            epkgs.tempel
          ];
          buildInputs = propagatedUserEnvPkgs;
        })
                                    ];
      });



        emacs-boost = pkgs.symlinkJoin {
        name = "emacs-booster";
        paths = [ my-emacs ];
        buildInputs = [pkgs.makeWrapper];
        postBuild = ''
        wrapProgram $out/bin/emacs \
          --prefix PATH : "${pkgs.emacs-lsp-booster}/bin" \
          --set LSP_USE_PLISTS true
        '';
        };
  in
  {
    packages.x86_64-linux.emacs = emacs-boost;

    packages.x86_64-linux.default = self.packages.x86_64-linux.emacs;

    devShells.x86_64-linux.default = pkgs.mkShell {
        buildInputs = [ emacs-boost pkgs.rustc pkgs.rust-analyzer pkgs.cargo pkgs.delta pkgs.ccls pkgs.pyright pkgs.black pkgs.fd pkgs.ripgrep ];
        shellHook = ''
            export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive";
        '';
    };
  };
}
