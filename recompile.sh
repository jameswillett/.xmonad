`ghc --make ~/.xmonad/xmonad.hs -package xmonad -i -ilib -dynamic -fforce-recomp -main-is main -v0 -o ~/.xmonad/xmonad-x86_64-linux; xmonad --restart ; echo 'recompiled!' | dzen2 -p 1;`
