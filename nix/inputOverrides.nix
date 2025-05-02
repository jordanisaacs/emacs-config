{ lib, ... }: {
  magit = _: super: {
    files = super.files // { "lisp/Makefile" = "Makefile"; };
  };

  lsp-snippet-tempel = _: super: {
    packageRequires = super.packageRequires // {
      tempel = "0";
      lsp-snippet = "0";
    };
    files =
      removeAttrs super.files [ "lsp-snippet-yasnippet.el" "lsp-snippet.el" ];
  };

  lsp-snippet = _: super: {
    files = removeAttrs super.files [
      "lsp-snippet-yasnippet.el"
      "lsp-snippet-tempel.el"
    ];
  };

  ace-window = _: super: {
    packageRequires = super.packageRequires // { transpose-frame = "0"; };
  };

  rustic = _: super: {
    packageRequires =
      builtins.removeAttrs super.packageRequires [ "flycheck" "projectile" ];
    files = builtins.removeAttrs super.files [ "rustic-flycheck.el" ];
  };

  gumshoe = _: super: {
    # packageRequires = builtins.removeAttrs super.packageRequires ["flycheck" "projectile"];
    files = builtins.removeAttrs super.files [ "gumshoe-persp.el" ];
  };

  magit-delta = _: super: {
    packageRequires = super.packageRequires // { dash = "0"; magit = "3"; };
  };
}
