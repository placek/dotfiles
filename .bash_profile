[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"

export HISTCONTROL=ignoreboth:erasedups

bind "set completion-ignore-case on"
bind "set show-all-if-ambiguous on"
if [[ $- == *i* ]]; then
  bind '"\e[A": history-search-backward'
  bind '"\e[B": history-search-forward'
fi

source "$HOME/.bash_plugins/git_completion.bash"

# prompt functions
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

# custom prompt
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

projects() {
  list=`tmux ls 2> /dev/null | cut -f1 -d':' | sort`

  case "$1" in
    "ls")
      echo $list
      ;;
    "add")
      target=`comm -3 <(ls -1 ~/Projects | sort) <(echo $list) | sort -n | fzf --prompt "Projects>"`
      tmux new-session -s $target -c ~/Projects/$target vim \; new-window
      ;;
    "new")
      target=$2
      if [ -z "$target" ]; then
        >&2 echo "usage: projects new <project-name>"
      else
        if [ -d ~/Projects/$target ]; then
          >&2 echo "project already exists"
        else
          mkdir -p ~/Projects/$2
          pushd ~/Projects/$target
            git init
          popd
          tmux new-session -s $target -c ~/Projects/$target vim \; new-window
        fi
      fi
      ;;
    *)
      if [ -n "$TMUX" ]; then
        >&2 echo "usage: projects [add|ls|help]"
      else
        tmux attach
      fi
      ;;
  esac
}

alias be="bundle exec"
alias bi="bundle install"
alias dcb="docker-compose build"
alias dcr="docker-compose run"
alias dcu="docker-compose up -d"
alias dcl="docker-compose logs"
alias dcd="docker-compose down"

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
export PATH="/usr/local/sbin:$PATH"
