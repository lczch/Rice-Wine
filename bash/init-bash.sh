#!/bin/sh
alias ec='emacsclient -c'

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    echo "Add ~/bin to PATH"
    PATH="$HOME/bin:$PATH"
fi

# my elisp script
if [ -d "$HOME/rice-wine/script" ] ; then
    echo "Add ~/rice-wine/script dir to PATH"
    PATH="$HOME/rice-wine/script:$PATH"
fi

# OPAM configuration
echo "Update opam config env"
. $HOME/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
