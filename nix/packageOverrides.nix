{ pkgs, ghostelModule, ghostelSrc, }:
final: prev: {
  jinx = prev.jinx.overrideAttrs (old:
    let moduleSuffix = pkgs.stdenv.targetPlatform.extensions.sharedLibrary;
    in {
      nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ pkgs.pkg-config ];
      buildInputs = (old.buildInputs or [ ]) ++ [ pkgs.enchant_2 ];
      preBuild = ''
	NIX_CFLAGS_COMPILE="$($PKG_CONFIG --cflags enchant-2) $NIX_CFLAGS_COMPILE"
	$CC -I. -O2 -fPIC -shared -o jinx-mod${moduleSuffix} jinx-mod.c -lenchant-2
      '';
    });

  magit = prev.magit.overrideAttrs (old: {
    preBuild = ''
      substituteInPlace Makefile --replace "include ../default.mk" ""
      make PKG=magit VERSION="${old.version}" magit-version.el
      rm Makefile
    '';
  });

  mathjax = prev.mathjax.overrideAttrs (old: {
    patchPhase = ''
      patchShebangs ./math2svg
    '';
    buildInputs = (old.buildInputs or [ ]) ++ [ pkgs.nodejs ];
  });

  lsp-mode = prev.lsp-mode.overrideAttrs (old: { LSP_USE_PLISTS = true; });

  ccls = prev.ccls.overrideAttrs (old: { LSP_USE_PLISTS = true; });

  # Ghostel ships a Zig-built native module (libghostty-vt bindings) that is
  # normally auto-downloaded at runtime.  The read-only Nix store blocks that,
  # so drop in the Nix-built module (see flake.nix:ghostelModule) next to
  # ghostel.el where `(locate-library "ghostel")` expects it.
  ghostel = prev.ghostel.overrideAttrs (old:
    let moduleSuffix = pkgs.stdenv.hostPlatform.extensions.sharedLibrary;
    in {
      src = ghostelSrc;
      preBuild = (old.preBuild or "") + ''
        # Drop the evil extension — evil isn't part of this configuration, so
        # byte-compiling it would fail on missing deps.
        rm -f evil-ghostel.el ghostel-evil.el

        install -m444 ${ghostelModule}/ghostel-module${moduleSuffix} \
          ghostel-module${moduleSuffix}
      '';
    });
}
