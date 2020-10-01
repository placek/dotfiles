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

# custom prompt
black="\[$(tput setaf 4)\]"
red="\[$(tput setaf 1)\]"
green="\[$(tput setaf 2)\]"
orange="\[$(tput setaf 3)\]"
blue="\[$(tput setaf 39)\]"
normal="\[$(tput sgr0)\]"

parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ \1/'
}

parse_git_status() {
  [ -n "$(git status --porcelain 2> /dev/null)" ] && echo "*"
}

PROMPT_COMMAND=__set_prompt

__set_prompt() {
  local exit=$?
  local cwd="${black}\w${normal}"
  local git_branch="${orange}\$(parse_git_branch)\$(parse_git_status)${normal}"
  PS1="${cwd}${git_branch} "
  if [[ $exit -eq 0 ]]; then
    PS1+="${green}$ ${normal}"
  else
    PS1+="${red}$ ${normal}"
  fi
}

# aliases
alias be="bundle exec"
alias bi="bundle install"
alias dcb="docker-compose build"
alias dcr="docker-compose run --rm"
alias dcu="docker-compose up -d"
alias dcl="docker-compose logs"
alias dcd="docker-compose down"
alias dsp="docker system prune"
alias dspv="docker system prune --volumes"
alias dcres="docker-compose restart"
alias dcps="docker-compose ps"
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# GIT
if [ -f "$HOME/.bash_plugins/git_completion.bash" ] ; then
  source "$HOME/.bash_plugins/git_completion.bash"
fi

# FZF
if [ -f "$HOME/.bash_plugins/fzf.bash" ] ; then
  source "$HOME/.bash_plugins/fzf.bash"
fi

# RVM
if [ -d "$HOME/.rvm" ] ; then
  [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
  export PATH="/usr/local/sbin:$PATH"
fi

# GHCUP
if [ -d "$HOME/.ghcup" ] ; then
  [ -f "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env" ] && source "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env"
fi

# NVM
if [ -d "$HOME/.nvm" ] ; then
  export NVM_DIR="$HOME/.nvm"
  [ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
  [ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && . "/usr/local/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion
fi

# LOCAL BINARIESs
if [ -d "$HOME/.local/bin" ] ; then
  export PATH="$HOME/.local/bin:$PATH"
fi
