export EDITOR=$(which emacs)

export RUSTC_WRAPPER=sccache
export JAVA_HOME=/usr/lib/jvm/default
export GEM_HOME="$(gem env user_gemhome)"

#path+=("$HOME/.config/emacs/bin")
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:$GEM_HOME/bin"
