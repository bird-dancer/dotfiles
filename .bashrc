alias docker_stop='sudo docker stop $(sudo docker ps -a -q)'
alias docker_rm='sudo docker rm $(sudo docker ps -a -q)'

alias em='/usr/bin/emacs -nw'

alias celar='clear'
alias lcear='clear'
alias lcear='clear'
alias clea='clear'
alias cleat='clear'

alias pac='sudo pacman -Syu'
alias pacr='sudo pacman -Rs'
alias paci='pacman -Si'
alias pacq='pacman -Qs'
alias cleanup='sudo guix system delete-generations; guix package -d; guix gc'

eval $(ssh-agent) > /dev/null

alias build='rm -rf build && cmake -B build -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=1 && make -C'

export CC=gcc CXX=g++
alias comp='gcc -std=c17 -Wall -Wextra -fstack-protector -g3 -lm'
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

alias b='cd ..'

alias ls='ls -F --color=auto'
alias la='ls -AF --color=auto'
alias ll='ls -lAghF --color=auto'

#alias grep='rg'
alias grep='grep --color=auto'

ex ()
{
    if [ -f "$1" ] ; then
        case $1 in
            *.tar.bz2)   tar xjf $1   ;;
            *.tar.gz)    tar xzf $1   ;;
            *.bz2)       bunzip2 $1   ;;
            *.rar)       unrar x $1   ;;
            *.gz)        gunzip $1    ;;
            *.tar)       tar xf $1    ;;
            *.tbz2)      tar xjf $1   ;;
            *.tgz)       tar xzf $1   ;;
            *.zip)       unzip $1     ;;
            *.Z)         uncompress $1;;
            *.7z)        7z x $1      ;;
            *.deb)       ar x $1      ;;
            *.tar.xz)    tar xf $1    ;;
            *.tar.zst)   unzstd $1    ;;
            *)           echo "'$1' cannot be extracted via ex()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

export XDG_RUNTIME_DIR=/run/user/$(id -u)

export TERM="xterm-256color"

source /usr/share/doc/pkgfile/command-not-found.bash

HISTSIZE=1000
HISTFILESIZE=2000  
HISTCONTROL=ignoreboth

bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'
#bind '"^[^M-f": history-search-backward'
#bind '"\e\C-m": history-search-forward'

source /usr/share/bash-complete-alias/complete_alias
#source /run/current-system/usr/share/bash-complete-alias/complete_alias
complete -F _complete_alias "${!BASH_ALIASES[@]}"

bind "set completion-ignore-case on"

shopt -s autocd # change to named directory
shopt -s cdspell # autocorrects cd misspellings
shopt -s cmdhist # save multi-line commands in history as single line
shopt -s dotglob
shopt -s histappend # do not overwrite history
shopt -s expand_aliases # expand aliases
shopt -s checkwinsize # checks term size when bash regains control

eval "$(starship init bash)"
