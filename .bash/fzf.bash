if [[ ! "$PATH" == *${HOME}/.vim/pack/bundle/opt/fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}${HOME}/.vim/pack/bundle/opt/fzf/bin"
fi

[[ $- == *i* ]] && source "${HOME}/.vim/pack/bundle/opt/fzf/shell/completion.bash" 2> /dev/null

source "${HOME}/.vim/pack/bundle/opt/fzf/shell/key-bindings.bash"
