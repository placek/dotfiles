[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

bind "set completion-ignore-case on"
bind "set show-all-if-ambiguous on"
if [[ $- == *i* ]]; then
  bind '"\e[A": history-search-backward'
  bind '"\e[B": history-search-forward'
fi

source "$HOME/.bash_plugins/git_completion.bash"

export PROMPT_COMMAND=__prompt_command

__prompt_command() {
  local exit=$?
  PS1="\[$(tput setaf 0)\]\w "
  if [[ $exit -eq 0 ]]; then
    PS1+="\[$(tput setaf 2)\]"
  else
    PS1+="\[$(tput setaf 1)\]"
  fi
  PS1+="\\$ \[$(tput sgr0)\]"
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

alias b="bundle exec"
export PATH="/usr/local/sbin:$PATH"
  export NVM_DIR="/Users/placek/.nvm"
  [ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
  [ -s "/usr/local/opt/nvm/etc/bash_completion" ] && . "/usr/local/opt/nvm/etc/bash_completion"  # This loads nvm bash_completion
