#!/bin/sh

numlockx on;
if [ ! -e "/home/james/.xmonad/music" ]; then
  ghc --make -dynamic /home/james/.xmonad/music.hs -o /home/james/.xmonad/music -outputdir /tmp;
  /home/james/.xmonad/recompile.sh;
fi
