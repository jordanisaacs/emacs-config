{
pkgs,
}: final: prev: {
jinx = prev.jinx.overrideAttrs (old: let
moduleSuffix = pkgs.stdenv.targetPlatform.extensions.sharedLibrary;
in {
nativeBuildInputs = (old.nativeBuildInputs or []) ++ [pkgs.pkg-config];
buildInputs = (old.buildInputs or []) ++ [pkgs.enchant2];
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

lsp-mode = prev.lsp-mode.overrideAttrs (old: {
LSP_USE_PLISTS=true;
});

ccls = prev.ccls.overrideAttrs (old: {
LSP_USE_PLISTS=true;
});
}
