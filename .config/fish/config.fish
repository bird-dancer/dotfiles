if status is-interactive
    # Commands to run in interactive sessions can go here
end

# load aliases
source ~/.config/fish/functions/aliases.fish

# editor
export EDITOR="which emacs"

# add to path
fish_add_path $HOME/.config/emacs/bin
fish_add_path $HOME/.local/bin

# ssh
eval (ssh-agent -c) > /dev/null

starship init fish | source
