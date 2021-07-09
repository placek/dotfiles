if not set --query FZF_DEFAULT_OPTS
  set --export FZF_DEFAULT_OPTS '--cycle --layout=reverse --border --height 90% --preview-window=wrap'
end

function __fzf_display_value_or_error --argument-names variable_name --description "Displays either the value of the variable passed in, or an informative message if it is not available."
  if set --query $variable_name
    echo $$variable_name
  else
    set_color red
    echo "$variable_name was not exported to this process so its value cannot be displayed." >&2
    set_color normal
  end
end

function __fzf_preview_file --argument-names file_path --description "Prints a preview for the given file based on its file type."
  if test -f "$file_path"
    bat --style=numbers --color=always "$file_path"
  else if test -d "$file_path"
    set --local CLICOLOR_FORCE true
    ls -a "$file_path"
  else if test -L "$file_path"
    set -l target_path (realpath $file_path)
    set_color yellow
    echo "'$file_path' is a symlink to '$target_path'."
    set_color normal
    __fzf_preview_file "$target_path"
  else if test -c "$file_path"
    __fzf_report_file_type "$file_path" "character device file"
  else if test -b "$file_path"
    __fzf_report_file_type "$file_path" "block device file"
  else if test -S "$file_path"
    __fzf_report_file_type "$file_path" "socket"
  else if test -p "$file_path"
    __fzf_report_file_type "$file_path" "named pipe"
  else
    echo "Unknown file type." >&2
  end
end

function __fzf_preview_todo --argument-names todo_id --description "Prints a preview for the given todo."
  bat --style=numbers --color=always --language=Markdown "$HOME/.todo/$todo_id"
end

function __fzf_report_file_type --argument-names file_path file_type --description "Explain the file type for a file."
  set_color red
  echo "Cannot preview '$file_path': it is a $file_type."
  set_color normal
end

function __fzf_search_current_dir --description "Search the current directory using fzf and fd. Insert the selected relative file path into the commandline at the cursor."
  set --local --export SHELL (command --search fish)
  set file_paths_selected (
    fd --hidden --follow --color=always --exclude=.git 2>/dev/null |
    fzf --multi --ansi --preview='__fzf_preview_file {}'
  )

  if test $status -eq 0
    for path in $file_paths_selected
      set escaped_path (string escape "$path")
      commandline --insert "$escaped_path "
    end
  end

  commandline --function repaint
end

function __fzf_search_todos --description "Search todos using fzf and fd. Insert the selected task ID into the commandline at the cursor."
  set --local --export SHELL (command --search fish)
  set todo_ids_selected (
    find $HOME/.todo -maxdepth 1 -type f 2>/dev/null | sed "s/^.*\///" |
    fzf --multi --ansi --preview='__fzf_preview_todo {}'
  )

  if test $status -eq 0
    for id in $todo_ids_selected
      set escaped_id (string escape "$id" | sed "s/^.*\///")
      commandline --insert "$escaped_id "
    end
  end

  commandline --function repaint
end

function __fzf_search_git_log --description "Search the git log of the current git repository. Insert the selected commit hash into the commandline at the cursor."
  if not git rev-parse --git-dir >/dev/null 2>&1
    echo '__fzf_search_git_log: Not in a git repository.' >&2
  else
    set --local --export SHELL (command --search fish)

    set selected_log_line (
      git log --color=always --format=format:'%C(bold blue)%h%C(reset) - %C(cyan)%as%C(reset) %C(yellow)%d%C(reset) %C(normal)%s%C(reset)  %C(dim normal)[%an]%C(reset)' | \
      fzf --ansi --tiebreak=index --preview='git show --color=always (string split --max 1 " " {})[1]'
    )
    if test $status -eq 0
      set abbreviated_commit_hash (string split --max 1 " " $selected_log_line)[1]
      set commit_hash (git rev-parse $abbreviated_commit_hash)
      commandline --insert $commit_hash
    end
  end

  commandline --function repaint
end

function __fzf_search_git_branch --description "Search the git branches of the current git repository. Insert the selected branch name into the commandline at the cursor."
  if not git rev-parse --git-dir >/dev/null 2>&1
    echo '__fzf_search_git_log: Not in a git repository.' >&2
  else
    set --local --export SHELL (command --search fish)

    set selected_branch_name (
      git branch --all --list --format '%(refname)' | \
      fzf --ansi --tiebreak=index --preview='git log --graph --pretty=format:"%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset" --color=always --abbrev-commit {}'
    )
    if test $status -eq 0
      set branch_name (string split --max 1 " " $selected_branch_name)[1]
      commandline --insert $branch_name
    end
  end

  commandline --function repaint
end

function __fzf_search_git_status --description "Search the git status of the current git repository. Insert the selected file paths into the commandline at the cursor."
  if not git rev-parse --git-dir >/dev/null 2>&1
    echo '__fzf_search_git_status: Not in a git repository.' >&2
  else
    set selected_paths (
      git -c color.status=always status --short |
      fzf --ansi --multi
    )
    if test $status -eq 0
      for path in $selected_paths
        if test (string sub --length 1 $path) = 'R'
          set cleaned_path (string split -- "-> " $path)[-1]
        else
          set cleaned_path (string sub --start=4 $path)
        end
        set cleaned_path_padded "$cleaned_path "
        commandline --insert $cleaned_path_padded
      end
    end
  end

  commandline --function repaint
end

function __fzf_search_history --description "Search command history using fzf. Replace the commandline with the selected command."
  history merge
  set command_with_ts (
    history --null --show-time="%m/%e %H:%M:%S | " |
    fzf --read0 --tiebreak=index --query=(commandline)
  )

  if test $status -eq 0
    set command_selected (string split --max 1 " | " $command_with_ts)[2]
    commandline --replace $command_selected
  end

  commandline --function repaint
end

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
    bind --mode insert \ct '__fzf_search_todos'
    bind --mode insert \e\cl '__fzf_search_git_log'
    bind --mode insert \e\cs '__fzf_search_git_status'
    bind --mode insert \e\cb '__fzf_search_git_branch'
  end
end
