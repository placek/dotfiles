# greetings
function fish_greeting
  if set -q TMUX
    echo (yellow) (date) (off)
  else
    inxi
  end
end
