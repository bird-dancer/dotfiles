#!/usr/bin/env sh

stow .

if [ "$ZSH_VERSION" ]; then
    autoload -U compinit
    compinit
fi

sudo localectl set-x11-keymap de

echo "finished"
