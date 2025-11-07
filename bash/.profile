test -z "$PROFILEREAD" && . /etc/profile || true

export LANGUAGE=en_US

eval $(ssh-agent) > /dev/null

export EDITOR=/usr/bin/emacs

export GHIDRA_INSTALL_DIR=/lib64/ghidra/
