export LC_CTYPE=en_US.UTF-8

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
alias cleanup='sudo pacman -Rns $(pacman -Qtdq)'

eval $(ssh-agent) > /dev/null

alias build='rm -rf build && cmake -B build -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=1 && make -C'

alias comp='gcc -std=c17 -Wall -Wextra -fstack-protector -g3 -lm'

alias b='cd ..'

alias ls='ls --color=auto'
alias la='ls -a --color=auto'
alias ll='ls -lagh --color=auto'

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

export TERM="xterm-256color"

source /usr/share/doc/pkgfile/command-not-found.bash

export HISTCONTROL=erasedups:ignorenoth

bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'
#bind '"^[^M-f": history-search-backward'
#bind '"\e\C-m": history-search-forward'

source /usr/share/bash-complete-alias/complete_alias
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
