# don't go further if this script was already processed
if [ -z ${MYENV_PROCESSED+x} ] ; then # include guard
export MYENV_PROCESSED=true

export PATH=$PATH:$HOME/bin:$HOME/local/bin:$HOME/.local/bin
export LSP_USE_PLISTS=true
export DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1
export DEBUGINFOD_URLS="https://debuginfod.archlinux.org"
export C4_EXTERN_DIR=~/proj/c4extern



#--------------------------------------------------------------------
# env
#

# read this file in the kde session:
# https://userbase.kde.org/Session_Environment_Variables
if [ -d $HOME/.config/plasma-workspace/env ] ; then
    if [ ! -L $HOME/.config/plasma-workspace/env/$(basename $(realpath ${BASH_SOURCE[0]})) ] ; then
        echo "Creating env in $HOME/.config/plasma-workspace/env/$(basename $(realpath ${BASH_SOURCE[0]}))"
        ln -fs $(realpath ${BASH_SOURCE[0]}) $HOME/.config/plasma-workspace/env/.
    fi
fi


#--------------------------------------------------------------------
# interactive

if [[ $- == *i* ]] ; then # is this interactive?

alias ll='ls -lFhp'
alias la='ls -lFhpA'
alias l='ls -CF'
alias st='git status'
alias di='git diff'
alias gitgr="git log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(bold yellow)%d%C(reset)' --all"
alias gitg='git gui &'
alias gitk='gitk --all &'
alias cd..='cd ..'
alias diff='diff --color=auto'
alias grep='grep --color=auto'
alias vnc='run_scaled --scale=1.75 vncviewer &'
alias vi=vim

export HISTSIZE=10000
export HISTFILESIZE=10000
export YAOURT_COLORS="nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"


#--------------------------------------------------------------------
# set prompt string

export mygrey='\[\033[0;37m\]'
export myred='\[\033[01;31m\]'
export myblue='\[\033[01;34m\]'
export mygreen='\[\033[01;32m\]'
export mynocolor='\[\033[0m\]'
export myboldwhite='\[\e[0;32m\]'
alias gitisrepo='[ "`which git 2>/dev/null`" != "" ] && [ "`git rev-parse --is-inside-work-tree 2>/dev/null`" == "true" ]'
alias gittersebranch="git branch --no-color 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/' -e 's/(HEAD detached at \(.*\))/\1/'"
alias gitterserepo="git config --get remote.origin.url | sed -e 's@.*/\(.*\)@\1@g' -e 's/.*:\(.*\)\.git$/\1/g'"
alias gitps1str='if `gitisrepo` ; then echo -n " (`gitterserepo`:`gittersebranch`)" ; fi'
alias ps1date='date +"%Y/%m/%d %H:%M:%S"'
PS1="\n$mygrey[\`ps1date\`]$mygrey $myblue(\w)$mygrey $myred(jobs:\j)$mygreen\`gitps1str\`$mynocolor \!\n$mygreen[\u@\h]$mygrey \$$mynocolor "

fi # is this interactive

fi # include guard
