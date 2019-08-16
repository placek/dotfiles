[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

bind "set completion-ignore-case on"
bind "set show-all-if-ambiguous on"
if [[ $- == *i* ]]; then
  bind '"\e[A": history-search-backward'
  bind '"\e[B": history-search-forward'
fi

source "$HOME/.bash_plugins/git_completion.bash"

export PS1="\[$(tput setaf 3)\]\w \\$ \[$(tput sgr0)\]"

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
    *)
      if [ -n "$TMUX" ]; then
        echo "usage: projects [add|ls|help]"
      else
        tmux attach
      fi
      ;;
  esac
}

alias b="bundle exec"
export PATH="/usr/local/sbin:$PATH"
