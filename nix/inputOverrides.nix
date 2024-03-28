{lib, ...}: {
  magit = _: super: {
    files =
      super.files
      // {
        "lisp/Makefile" = "Makefile";
      };
  };

  lsp-snippet-tempel = _: super: {
    packageRequires = super.packageRequires // {
      tempel = "0";
    };
    files = removeAttrs super.files [
      "lsp-snippet-yasnippet.el"
    ];
  };

  rustic = _: super: {
    packageRequires = builtins.removeAttrs super.packageRequires ["flycheck" "projectile"];
    files = builtins.removeAttrs super.files [
      "rustic-flycheck.el"
    ];
  };

  gumshoe = _: super: {
    # packageRequires = builtins.removeAttrs super.packageRequires ["flycheck" "projectile"];
    files = builtins.removeAttrs super.files [
      "gumshoe-persp.el"
    ];
  };

}
