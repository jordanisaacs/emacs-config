{ pkgs }:

pkgs.emacs-git-pgtk.overrideAttrs (old: {
  patches = (old.patches or [ ]) ++ [
    ./patches/eln-cache-correct-spot.patch
    ./patches/tty-synchronized-output.patch
  ];
})
