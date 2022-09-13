# greetings
function fish_greeting
  if set -q TMUX
    date
  else
    inxi
  end
end
