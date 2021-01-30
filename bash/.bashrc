## bash settings

# set locale
export LANG=zh_CN.UTF8
# export LANG=zh_CN.GB18030

# including scripts in emacs-lisp
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

# haskell
export STACK_ROOT="C:\\msys64\\home\\lzh\\.stack"

# java
PATH="C:\\Program Files\\Java\\jdk1.8.0_211\\bin:$PATH"

# ImageMagick
if [ -d "$HOME/bin/ImageMagick" ] ; then
    PATH="$HOME/bin/ImageMagick:$PATH"
fi


# sbcl
export SBCL_HOME=~/bin

# Launch Zsh
# if [ -t 1 ]; then
#    echo "Start zsh"
#    exec zsh
# fi

# turn off git information in bash, very slow
function git_prompt_info() {
    ref=$(git symbolic-ref HEAD 2> /dev/null) || return
    echo "$OSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$OSH_THEME_GIT_PROMPT_SUFFIX"
}
