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