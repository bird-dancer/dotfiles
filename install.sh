#!/usr/bin/env sh

stow --adopt .
git reset --hard

"$HOME/.config/emacs.d/bin/doom" sync -u
"$HOME/.config/emacs.d/bin/doom" env

#sudo localectl set-x12-keymap de

echo "finished now just run source $HOME/.zshrc"
