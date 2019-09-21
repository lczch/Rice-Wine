## bash settings

# set locale
export LANG=zh_CN.UTF8

if [ -d "$HOME/rice-wine/bin" ] ; then
    echo "Add ~/rice-wine/bin dir to PATH"
    PATH="$HOME/rice-wine/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    echo "Add ~/bin to PATH"
    PATH="$HOME/bin:$PATH"
fi

# git flow completion
# echo "turn on git completion."
# source "$HOME/rice-wine/bash/git-completion"

# coq
# if [ -d "$HOME/bin/Coq8.4/bin" ] ; then
#     PATH="$HOME/bin/Coq8.4/bin:$PATH"
# fi

if [ -d "$HOME/bin/Coq8.6.1/bin" ] ; then
    PATH="$HOME/bin/Coq8.6.1/bin:$PATH"
fi
# texlive for latex
# windows中的绝对路径必须使用window的路径方式, 不知道为什么
PATH="C:\\texlive\\2018\\bin\\win32:$PATH"

# specify emacs server file manually, necessary on windows
export EMACS_SERVER_FILE="$HOME/rice-wine/server/server"

# specify editor used for git
export editor=et

# cargo of rust
if [ -d "C:\\Users\\lzh\\.cargo\\bin" ] ; then
    echo "Rust: Add .cargo/bin to PATH"
    PATH="C:\\Users\\lzh\\.cargo\\bin:$PATH"
fi

if [ -d "$HOME/bin/nodejs" ] ; then
    PATH="$HOME/bin/nodejs:$HOME/bin/nodejs/node_global:$PATH"
fi

# java
PATH="C:\\Program Files\\Java\\jdk-11.0.2\\bin:$PATH"

# sbcl
export SBCL_HOME=~/bin

# Launch Zsh
if [ -t 1 ]; then
    echo "Start zsh"
    exec zsh
fi
