if not set --query fzf_fish_custom_keybindings
  bind \cf '__fzf_search_current_dir'
  bind \cr '__fzf_search_history'
  bind \cv '__fzf_search_shell_variables'
  bind \e\cl '__fzf_search_git_log'
  bind \e\cs '__fzf_search_git_status'

  if test "$fish_key_bindings" = 'fish_vi_key_bindings'
    bind --mode insert \cf '__fzf_search_current_dir'
    bind --mode insert \cr '__fzf_search_history'
    bind --mode insert \cv '__fzf_search_shell_variables'
    bind --mode insert \e\cl '__fzf_search_git_log'
    bind --mode insert \e\cs '__fzf_search_git_status'
  end
end

if not set --query FZF_DEFAULT_OPTS
  set --export FZF_DEFAULT_OPTS '--cycle --layout=reverse --border --height 90% --preview-window=wrap'
end
