export VISUAL=vim
export EDITOR="$VISUAL"

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

rotate_eternal_history() {
    local max_bytes=$((20 * 1024 * 1024)) size archive
    [[ -f $BASH_ETERNAL_HISTORY ]] || return 0
    size=$(stat -c %s "$BASH_ETERNAL_HISTORY") || return 0
    (( size < max_bytes )) && return 0
    archive="$BASH_ETERNAL_HISTORY_ARCHIVE/$(date +%Y-%m-%d_%H%M%S).tsv"
    mkdir -p "$BASH_ETERNAL_HISTORY_ARCHIVE" \
        && mv "$BASH_ETERNAL_HISTORY" "$archive" 2>/dev/null \
        && gzip "$archive"
}
