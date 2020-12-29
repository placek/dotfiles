function __fzf_search_shell_variables --description "Search and inspect shell variables using fzf. Insert the selected variable into the commandline at the cursor."
  set --local --export SHELL (command --search fish)

  set variable_name (
    set --names |
    fzf --preview '__fzf_display_value_or_error {}'
  )

  if test $status -eq 0
    commandline --insert $variable_name
  end

  commandline --function repaint
end
