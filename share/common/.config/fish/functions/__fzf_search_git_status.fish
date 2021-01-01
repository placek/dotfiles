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
