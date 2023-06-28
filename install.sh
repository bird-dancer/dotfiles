#!/usr/bin/env sh

stow --adopt .
git reset --hard

#sudo localectl set-x11-keymap de

echo "finished now just run source your new config"
