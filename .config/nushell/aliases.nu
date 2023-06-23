alias la = ls
alias ls = exa --icons -a
alias ll = ls -al
alias tree = exa --tree --icons
alias ssr = ssh pi@raspberrypi
alias cat = bat
alias r = ranger
alias b = cd ..
alias speed = wget --output-document=/dev/null https://speed.hetzner.de/1GB.bin


# c building compiling
#alias build = rm -rf build and cmake -B build -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=1 and make -C build
alias comp = gcc -std=c17 -Wall -fstack-protector -g3 -lm

# ssh
#ssh-agent > /dev/null

# update stuff
alias pac = sudo pacman -Syu
alias pacr = sudo pacman -Rs

# typing errors
alias lear  = clear
alias celar = clear
alias lcear = clear
