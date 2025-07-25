test -z "$PROFILEREAD" && . /etc/profile || true

[[ -r /etc/bash.bashrc ]] && . /etc/bash.bashrc

alias docker_stop='[ -n "$(sudo docker ps -a -q)" ] && sudo docker stop $(sudo docker ps -a -q) || echo "No containers to stop"'
alias docker_rm='[ -n "$(sudo docker ps -a -q)" ] && sudo docker rm $(sudo docker ps -a -q) || echo "No containers to remove"'

alias em='emacs -nw'

alias celar='clear'
alias lcear='clear'
alias lcear='clear'
alias clea='clear'
alias cleat='clear'

alias gcleanup='sudo guix system delete-generations; guix package -d; guix gc'

alias nup='nix-channel --update; nix-env -u'

alias nclean='nix-env --delete-generations +1; nix-collect-garbage'

alias zinf='zypper info --provides --recommends --requires --suggests'
alias zse='zypper search'
alias zin='sudo zypper install'
alias zrm='sudo zypper remove -u'
alias zup='sudo zypper update'
alias zd='sudo zypper dup'
alias zre='sudo zypper refresh'
alias zref='sudo zypper refresh -f'
alias zps='sudo zypper ps -s'
alias zun='zypper packages --unneeded'
alias zlo='sudo zypper addlock'

eval $(ssh-agent) > /dev/null

alias build='rm -rf build && cmake -B build -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=1 && make -C'

alias make='make -j $(nproc)'

export CC=gcc CXX=g++
alias comp='gcc -std=c17 -Wall -Wextra -fstack-protector -g3 -lm'
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

alias b='cd ..'

alias ls='ls -F --color=auto'
alias la='ls -AF --color=auto'
alias ll='ls -lAghF --color=auto'

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

alias wget='wget2'

export TERM="xterm-256color"
#export TERM=konsole

export HISTSIZE=1000
export HISTFILESIZE=2000  
export HISTCONTROL=ignoreboth:erasedups

bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

source ~/.dotfiles/complete-alias/complete_alias
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

export CC=gcc CXX=g++

export LC_CTYPE=en_US.UTF-8

if command -v emacs > /dev/null ; then
    export EDITOR=$(command -v emacs)
fi

export GUIX_PROFILE="$HOME/.guix-profile"
if [ -d "$HOME/.config/guix/current/bin" ]; then
    export PATH="$HOME/.config/guix/current/bin:$PATH"
fi
if [ -d "$HOME/.guix-profile" ]; then
    export PATH="$HOME/.guix-profile/bin:$PATH"
fi
if [ -d "$GUIX_PROFILE/etc/profile" ]; then
    . "$GUIX_PROFILE/etc/profile"
fi
if [ -d "$GUIX_PROFILE/lib" ]; then
    export "LD_LIBRARY_PATH=$GUIX_PROFILE/lib:$LD_LIBRARY_PATH"
fi

export NIX_PATH=nixpkgs=channel:nixos-unstable

export GHIDRA_INSTALL_DIR=/lib64/ghidra/

if [ -d "$HOME/.cargo" ] ; then
    export PATH="$HOME/.cargo/bin:$PATH"
fi
if [ -d "$HOME/.emacs.d/bin" ] ; then
    export PATH="$HOME/.emacs.d/bin:$PATH"
fi
if [ -d "$HOME/.local/bin" ] ; then
    export PATH="$PATH:$HOME/.local/bin"
fi
if [ -d "$GEM_HOME/bin" ] ; then
    export PATH="$PATH:$GEM_HOME/bin"
fi
if [ -d "$HOME/go/bin" ] ; then
    export PATH="$PATH:$HOME/go/bin"
fi
if [ -d "$HOME/arm/bin" ] ; then
    export PATH=$HOME/arm/bin:$PATH
fi
export PATH=/usr/share/zap/:$PATH
