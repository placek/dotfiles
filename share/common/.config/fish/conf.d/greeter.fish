# greetings
function fish_greeting
  if set -q TMUX
    echo (yellow) (date) (off)
  else
    inxi
    echo (yellow)"-- TODO --" (off)
    todoist --color --indent --namespace --header list
  end
end
