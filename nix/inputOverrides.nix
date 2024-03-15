{lib, ...}: {
  magit = _: prev: {
    files =
      prev.files
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
}
