# exports
export BASH_SILENCE_DEPRECATION_WARNING=1
export HISTCONTROL=ignoreboth:erasedups
export EDITOR=vim
export HISTSIZE=1000
export HISTFILESIZE=2000
export PROJECTS_DIR=${HOME}/Projects

# bash options
bind "set completion-ignore-case on"
bind "set show-all-if-ambiguous on"
if [[ $- == *i* ]]; then
  bind '"\e[A": history-search-backward'
  bind '"\e[B": history-search-forward'
fi
shopt -s checkwinsize
shopt -s histappend
shopt -s cmdhist

if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# NIX-PROFILE
if [ -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ] ; then
  source "$HOME/.nix-profile/etc/profile.d/nix.sh"
  if type nix-env > /dev/null; then
    export LOCALE_ARCHIVE=`nix-env --installed --no-name --out-path --query glibc-locales`/lib/locale/locale-archive
  fi
fi

# local binaries
if [ -d "$HOME/.local/bin" ] ; then
  export PATH="$PATH:$HOME/.local/bin"
fi
