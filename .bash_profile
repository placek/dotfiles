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

# GIT
if [ -f "$HOME/.bash/git_completion.bash" ] ; then
  source "$HOME/.bash/git_completion.bash"
fi

# FZF
if [ -f "$HOME/.bash/fzf.bash" ] ; then
  source "$HOME/.bash/fzf.bash"
fi

# RVM
if [ -d "$HOME/.rvm" ] ; then
  [ -s "$HOME/.rvm/scripts/rvm" ] && source "$HOME/.rvm/scripts/rvm"
  export PATH="/usr/local/sbin:$PATH"
fi

# GHCUP
if [ -d "$HOME/.ghcup" ] ; then
  [ -f "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env" ] && source "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env"
fi

# NVM
if [ -d "$HOME/.nvm" ] ; then
  export NVM_DIR="$HOME/.nvm"
  [ -s "/usr/local/opt/nvm/nvm.sh" ] && source "/usr/local/opt/nvm/nvm.sh"
  [ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && source "/usr/local/opt/nvm/etc/bash_completion.d/nvm"
fi

# prompt
if [ -f "$HOME/.bash/prompt.bash" ] ; then
  source "$HOME/.bash/prompt.bash"
fi

# aliases
if [ -f "$HOME/.bash/aliases.bash" ] ; then
  source "$HOME/.bash/aliases.bash"
fi

# local binaries
if [ -d "$HOME/.local/bin" ] ; then
  export PATH="$PATH:$HOME/.local/bin"
fi
