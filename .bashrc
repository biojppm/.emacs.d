
# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

alias st='git status'
alias di='git diff'


# -----------------------------------------------------------------------------
# rsync utils
# https://stackoverflow.com/questions/4549945/is-it-possible-to-specify-a-different-ssh-port-when-using-rsync

function rsync_upload()
{
    dir_local="$1"
    dir_remote="$2"
    user_and_host="$3"
    port="$4"
    _rsync_cmd -e "ssh -p $port" $dir_local $user_and_host:$dir_remote
}

function rsync_download()
{
    dir_local="$1"
    dir_remote="$2"
    user_and_host="$3"
    port="$4"
    _rsync_cmd -e "ssh -p $port" $user_and_host:$dir_remote $dir_local
}

function _rsync_cmd()
{
    # regarding "$@":
    # see https://stackoverflow.com/questions/3898665/what-is-in-bash
    # see https://www.thegeekstuff.com/2010/05/bash-shell-special-parameters/
    rsync -r -t -p -v --progress --ignore-existing -c -i -s "$@"
}
