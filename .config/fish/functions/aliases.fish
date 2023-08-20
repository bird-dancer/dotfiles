alias ls="exa --icons -a"
alias ll="exa --icons -la"
alias tree="exa --tree --icons"
alias ssr="ssh pi@raspberrypi"
alias b="cd .."
alias cat="bat"
#alias grep="rg"
alias speed="wget --output-document=/dev/null https://speed.hetzner.de/1GB.bin"

# building compiling
alias build="rm -rf build && cmake -B build -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=1 && make -C build"
alias comp="gcc -std=c17 -Wall -fstack-protector -g3 -lm"

# ssh
# eval $(ssh-agent) > /dev/null

# updates
alias pac="sudo pacman -Syu"
alias pacr="sudo pacman -Rs"
alias paci="pacman -Si"
# typing errors
alias celar="clear"
alias lcear="clear"
alias lcear="clear"

# ex - archive extractor
function ex -d "extract an archive"
    if [ -f $1 ]
       switch $1
        case *.tar.bz2
             tar xjf $1
        case *.tar.gz
             tar xzf $1
        case *.tar.xz
             tar xJf $1
        case *.bz2
             bunzip2 $1
        case *.rar
             unrar x $1
        case *.gz
             gunzip $1
        case *.tar
             tar xf $1
        case *.tbz2
             tar xjf $1
        case *.tgz
             tar xzf $1
        case *.zip
             unzip $1
        case *.Z
             uncompress $1
        case *.7z
             7z x $1
        case *
             echo "'$1' cannot be extracted via ex"
        end
    else
        echo "'$1' is not a valid file"
    end
end
