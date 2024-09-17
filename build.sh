#! /usr/bin/env nix-shell
#! nix-shell -i bash -p "ghc.withPackages (ps: [ps.xmonad ps.xmonad-contrib])"

# This script is symlinked to ~/.xmonad/build.
#
# Type mod-q to restart xmonad, recompiling the code if necessary.

cd /home/thu/projects/xmonad/

ghc \
  -i/home/thu/projects/xmonad \
  --make /home/thu/projects/xmonad/xmonad.hs \
  -o /home/thu/.xmonad/xmonad-x86_64-linux

rm -f *.o *.hi
