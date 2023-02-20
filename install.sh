#!/usr/bin/env sh

stow --adopt .
git reset --hard

#git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
#~/.emacs.d/bin/doom install

"$HOME/.emacs.d/bin/doom" sync -u
"$HOME/.emacs.d/bin/doom" env

sudo localectl set-x11-keymap de

echo "finished now just run source $HOME/.zshrc"
