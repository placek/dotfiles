set -q SYMBOL || set SYMBOL '$'

set __fish_git_prompt_show_informative_status 1
set __fish_git_prompt_showstashstate 1
set __fish_git_prompt_showcolorhints 1
set __fish_git_prompt_char_stateseparator " "
set __fish_git_prompt_color_branch yellow
set __fish_git_prompt_color_prefix brblack
set __fish_git_prompt_color_suffix brblack

function nix_prompt
  if test "$IN_NIX_SHELL" != ""
    set_color brblack
    echo -n -s " nix"
    set_color normal
  end
end

function symbol_prompt
  if string match -qv 0 $__pipestatus
    set_color -o red
    echo -n -s " $SYMBOL"
    set_color normal
    set_color brblack
    echo -n -s " $__pipestatus"
  else
    set_color -o green
    echo -n -s " $SYMBOL"
  end
  set_color normal
end

function fish_prompt
  set -lx __pipestatus $pipestatus

  fish_vcs_prompt
  nix_prompt
  symbol_prompt

  echo -n -s " "
end

function fish_right_prompt
  set_color brblack
  echo -n -s (prompt_pwd)" "(date +%H%M%S)
  set_color normal
end
