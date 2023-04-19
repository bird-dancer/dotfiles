# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/felix/.zshrc'
autoload -Uz compinit
compinit
# End of lines added by compinstall

# format
export LC_CTYPE=en_US.UTF-8

# editor
export EDITOR="which emacs"

# alias
[ -f "$HOME/.aliasrc" ] && source "$HOME/.aliasrc"

function cd {
	builtin cd "$@" && ls
}
ls
path+=("$HOME/.config/emacs/bin")
export path
# plugins
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh 2>/dev/null
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null

# starship
eval "$(starship init zsh)"
