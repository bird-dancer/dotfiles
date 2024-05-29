[[ -r ~/.bashrc ]] && . ~/.bashrc
  GUIX_PROFILE="$HOME/.config/guix/current"
. "$GUIX_PROFILE/etc/profile"

if [ -z "$(which emacs)" ] ; then
    export EDITOR=$(which emacs)
fi

export CC=gcc CXX=g++
if [ -z "$(which sccache)" ] ; then
    export RUSTC_WRAPPER=sccache
fi
if [ -d "/usr/lib/jvm/default" ] ; then
    export JAVA_HOME=/usr/lib/jvm/default
fi
if [ -z "$(which gem)" ] ; then
    export GEM_HOME="$(gem env user_gemhome)"
fi

export LC_CTYPE=en_US.UTF-8

export PATH="$HOME/sccache/target/release:$PATH"
if [ -d "$HOME/.emacs.d/bin" ] ; then
    export PATH="$HOME/.emacs.d/bin:$PATH"
fi
if [ -d "$HOME/.local/bin" ] ; then
    export PATH="$PATH:$HOME/.local/bin"
fi
if [ -d "$GEM_HOME/bin" ] ; then
    export PATH="$PATH:$GEM_HOME/bin"
fi
