#!/usr/bin/bash

emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "Emacs.org")'

# Find all .org files in the specified directory and its subdirectories
org_files=$(find . -type f -name "*.org")

# Loop through each .org file and tangle it using Emacs batch mode
for org_file in $org_files; do
    echo "Tangling $org_file..."
    emacs --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \"$org_file\")"
done

echo "Tangling complete for all .org files."


#stow --adopt .
#git reset --hard

#sudo localectl set-x11-keymap de

echo "finished now just source your new config"
