{ pkgs, pmSrc, }:
final: prev: {
  # Build pm from a local worktree (see flake.nix `pm-src' input) so
  # uncommitted changes show up. The recipe (`recipes/pm') still
  # controls `:files' filtering; only the src is swapped.
  #
  # The `path:' input lands as a store symlink whose target lives in
  # /home — trivialBuild's unpackPhase can't follow it inside the
  # sandbox. `builtins.path' rematerializes the contents into a real
  # store directory at eval time, before any sandboxed build runs.
  pm = prev.pm.overrideAttrs (_: {
    src = builtins.path { path = pmSrc; name = "pm-src"; };
  });

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
}
