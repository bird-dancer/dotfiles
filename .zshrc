alias ls='exa --icons'
alias la='exa -a --icons'
alias ll='exa --icons -lagh'
alias tree='exa --tree --icons'

alias grep='rg'

alias cat='bat'

alias b='cd ..'

alias build='rm -rf build && cmake -B build -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=1 && make -C'

alias comp='gcc -std=c17 -Wall -Wextra -fstack-protector -g3 -lm'

eval $(ssh-agent) > /dev/null

alias pac='sudo pacman -Syu'
alias pacr='sudo pacman -Rs'
alias paci='pacman -Si'

alias celar='clear'
alias lcear='clear'
alias lcear='clear'
alias clea='clear'
alias cleat='clear'

ex (){
        if [ -f $1 ] ; then
                case $1 in
                        *.tar.bz2)   tar xjf $1   ;;
                        *.tar.gz)    tar xzf $1   ;;
                        *.tar.xz)    tar xJf $1   ;;
                        *.bz2)       bunzip2 $1   ;;
                        *.rar)       unrar x $1     ;;
                        *.gz)        gunzip $1    ;;
                        *.tar)       tar xf $1    ;;
                        *.tbz2)      tar xjf $1   ;;
                        *.tgz)       tar xzf $1   ;;
                        *.zip)       unzip $1     ;;
                        *.Z)         uncompress $1;;
                        *.7z)        7z x $1      ;;
                        *)      echo "'$1' cannot be extracted via ex()" ;;
                esac
        else
                echo "'$1' is not a valid file"
        fi
}

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

bindkey -e

# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename "$HOME/.zshrc"
autoload -Uz compinit
compinit
# End of lines added by compinstall

export LC_CTYPE=en_US.UTF-8

function cd {
        builtin cd "$@" && ls
}
ls

eval "$(starship init zsh)"

export EDITOR=$(which emacs)

export RUSTC_WRAPPER=sccache
export JAVA_HOME=/usr/lib/jvm/default
export GEM_HOME=$HOME/.local/share/gem

path+=("$HOME/.config/emacs/bin")
path+=("$HOME/.local/bin")
path+=("$HOME/.local/share/gem/ruby/3.0.0/bin")
path+=("$HOME/arm/bin")

source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh 2>/dev/null

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null

source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh 2> /dev/null

fpath=(/usr/share/zsh/site-functions $fpath)
