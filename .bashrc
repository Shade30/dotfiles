# ~/.bashrc: executed by bash(1) for interactive shells.

# User dependent .bashrc file

# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

if ! shopt -q login_shell; then # We're not a login shell
    # Arch Linux workaround: provide append_path API function to scripts in /etc/profile.d

    # Append "$1" to $PATH when not already in.
    # This function API is accessible to scripts in /etc/profile.d
    append_path () { case ":$PATH:" in *:"$1":*) ;; *) PATH="${PATH:+$PATH:}$1" ;; esac; }

    for script in /etc/profile.d/*.sh; do
        [[ -r $script ]] && . "$script"
    done

    # Unload our profile API functions
    unset -f append_path
fi

# Shell settings
#
# recalculate LINES/COLUMNS after command
shopt -s checkwinsize
# ** recursively, ls **/*.java
shopt -s globstar
# warn about background tasks
shopt -s checkjobs
# > does not rewrite file, >| is required
set -o noclobber
# Disable XON/XOFF to enable search forward
[[ $- == *i* ]] && stty -ixon
# vim as editor
export VISUAL=vim
export EDITOR="$VISUAL"

# History
#
# Session history size, default 500
#HISTSIZE=10000
# Max lines in history file, default HISTSIZE
#HISTFILESIZE=20000
# No duplicates, no commands prepended by space
HISTCONTROL=ignoreboth:erasedups
# Make bash append rather than overwrite the history on disk
shopt -s histappend
# Bash eternal history
BASH_ETERNAL_HISTORY=~/.bash_eternal_history
PROMPT_COMMAND+=('history -a')
PROMPT_COMMAND+=('printf "%s\t%s\t%s\tscreen %s\t%s\t%s\t%s\n" "$$" "$USER" "$HOSTNAME" "$WINDOW" "$(date "+%D %T %Y %s")" "$PWD" "$(history 1)" >> "$BASH_ETERNAL_HISTORY"')

# Bash prompt
#
PS1="\[\033[01;34m\]#\u@\h:\w:[\${PIPESTATUS[@]}]\$\[\033[00m\] "

# Umask
#
# Personal machine (primary group == user name): group-writable files; shared server: 022
if [ "$(id -gn)" == "$(id -un)" -a $(id -u) -gt 99 ]; then
    umask 002
else
    umask 022
fi

# Aliases
#
# Interactive operation...
alias rm='rm -I'                              # asks only for 4 files and more
alias cp='cp -i'
alias mv='mv -i'

# Default to human readable figures
alias df='df -h'
alias du='du -h'

# Misc
alias less='less -R'                          # raw control characters
alias whence='type -a'                        # where, of a sort
alias grep='grep --color'                     # show differences in colour
alias egrep='grep -E --color=auto'            # show differences in colour
alias fgrep='fgrep --color=auto'              # show differences in colour
export GREP_COLORS='mt=1;31'                  # green for matches
alias ic='iconv -f cp866 -t utf-8'            # shortcuts for cmd tools output conversion

# Some shortcuts for different directory listings
alias ls='ls -hF --color=tty'                 # classify files in colour
alias ll='ls -larth'                          # long list
alias la='ls -A'                              # all but . and ..
alias l='ls -CF'                              #

# Functions
#
# from https://gist.github.com/lonnen/3101795
# from i8ramin - http://getintothis.com/blog/2012/04/02/git-grep-and-blame-bash-function/
# runs git grep on a pattern, and then uses git blame to who did it
ggb() {
    git grep -n "$1" | while IFS=: read i j k; do git blame -L $j,$j $i | cat; done
}

# shows filename
ggbf() {
    git grep -n "$1" | while IFS=: read i j k; do git blame -L $j,$j $i | cat | sed 's|^|'$i':\t|'; done
}

# small modification for git egrep bash
geb() {
    git grep -E -n "$1" | while IFS=: read i j k; do git blame -L $j,$j $i | cat; done
}

# source: https://madebynathan.com/2011/10/04/a-nicer-way-to-use-xclip/
# A shortcut function that simplifies usage of xclip/wl-copy.
# - Accepts input from either stdin (pipe), or params.
# ------------------------------------------------
cb() {
  local _scs_col="\e[0;32m"; local _wrn_col='\e[1;31m'; local _trn_col='\e[0;33m'
  # Check that xclip is installed.
  if [ ${XDG_SESSION_TYPE} == "x11" ] && ! type xclip > /dev/null 2>&1; then
    echo -e "$_wrn_col""You must have the 'xclip' package installed.\e[0m"
  # Check that wl-clipboard is installed.
  elif [ ${XDG_SESSION_TYPE} == "wayland" ] && ! type wl-copy > /dev/null 2>&1; then
    echo -e "$_wrn_col""You must have the 'wl-clipboard' package installed.\e[0m"
  # Check user is not root (root doesn't have access to user xorg server)
  elif [[ "$USER" == "root" ]]; then
    echo -e "$_wrn_col""Must be regular user (not root) to copy a file to the clipboard.\e[0m"
  else
    # If no tty, data should be available on stdin
    if ! [[ "$( tty )" == /dev/* ]]; then
      input="$(< /dev/stdin)"
    # Else, fetch input from params
    else
      input="$*"
    fi
    if [ -z "$input" ]; then  # If no input, print usage message.
      echo "Copies a string to the clipboard."
      echo "Usage: cb <string>"
      echo "       echo <string> | cb"
    else
      # Copy input to clipboard
      if [ ${XDG_SESSION_TYPE} == "x11" ]; then
        echo -n "$input" | xclip -selection c
      elif [ ${XDG_SESSION_TYPE} == "wayland" ]; then
        echo -n "$input" | wl-copy -n
      fi
      # Truncate text for status
      if [ ${#input} -gt 80 ]; then input="$(echo $input | cut -c1-80)$_trn_col...\e[0m"; fi
      # Print status.
      echo -e "$_scs_col""Copied to clipboard:\e[0m $input"
    fi
  fi
}

# diff with emacs
# source: https://emacs.stackexchange.com/a/34952
ediff () 
{ 
    if [ -d $1 ]; then
        emacsclient -c -a emacs -q --eval "(ediff-directories \"$1\" \"$2\" \"\")";
    else
        emacsclient -c -a emacs -q --eval "(ediff-files \"$1\" \"$2\")";
    fi
}
ediff3 () 
{ 
    if [ $# -ne 3 ]; then
        echo Usage: $0 local base other;
        exit 1;
    fi;
    emacsclient -c -a emacs -q --eval '(ediff-merge-with-ancestor "'$1'" "'$2'" "'$3'")'
}

BASH_ETERNAL_HISTORY_ARCHIVE=~/.bash_eternal_history.d
# Gzipped copy of the eternal history, the source file is kept as is
archive_eternal_history() {
    local archive="$BASH_ETERNAL_HISTORY_ARCHIVE/$(date +%Y-%m-%d_%H%M%S).tsv.gz"
    [[ -f $BASH_ETERNAL_HISTORY ]] || return 0
    mkdir -p "$BASH_ETERNAL_HISTORY_ARCHIVE" \
        && gzip -c "$BASH_ETERNAL_HISTORY" > "$archive" \
        && echo "$archive"
}

# Keep commands that were run at least twice, one line per command (the latest one).
# Lines appended by other shells while awk is running are carried over untouched.
compact_eternal_history() {
    local file=$BASH_ETERNAL_HISTORY lines tmp
    [[ -f $file ]] || return 0
    lines=$(wc -l < "$file") || return 1
    tmp=$(mktemp "$file.XXXXXX") || return 1
    awk -F'\t' -v lines="$lines" '
        function cmd_text(field) { sub(/^ *[0-9]+ +/, "", field); return field }
        FNR > lines { next }
        NR == FNR { key = cmd_text($7); count[key]++; last[key] = FNR; next }
        { key = cmd_text($7) }
        count[key] >= 2 && last[key] == FNR
    ' "$file" "$file" > "$tmp" \
        && tail -n +"$((lines + 1))" "$file" >> "$tmp" \
        && mv "$tmp" "$file" \
        && echo "$lines -> $(wc -l < "$file") lines"
}

## Define any user-specific variables you want here.
[[ -f ~/.bash_functions ]] && source ~/.bash_functions
[[ -f ~/.bashrc_custom ]] && source ~/.bashrc_custom
