set -gx EDITOR vim
set -gx PROJECTS_DIR "$HOME/Projects"
set -gx NIXFILES_DIR "$HOME/Projects/nixfiles"
set -gx TODO_DIR "$HOME/.todo"

# pfetch greetings
function fish_greeting
  inxi
  echo (yellow)"-- TODO --" (off)
  todo list !+done
end

# unbind Ctrl-V
bind -e \cv

# abbreviations
abbr --add dsp "docker system prune"
abbr --add dspv "docker system prune --volumes"
abbr --add dcb "docker-compose build"
abbr --add dcr "docker-compose run --rm"
abbr --add dcu "docker-compose up -d"
abbr --add dcl "docker-compose logs"
abbr --add dcd "docker-compose down --remove-orphans"
abbr --add dcres "docker-compose restart"
abbr --add dcps "docker-compose ps"

abbr --add lcb "docker-compose -f .local.compose build"
abbr --add lcr "docker-compose -f .local.compose run --rm"
abbr --add lcrt "docker-compose -f .local.compose run --rm test"
abbr --add lcrw "docker-compose -f .local.compose run --rm web"
abbr --add lcu "docker-compose -f .local.compose up -d"
abbr --add lcl "docker-compose -f .local.compose logs"
abbr --add lcd "docker-compose -f .local.compose down --remove-orphans"
abbr --add lcres "docker-compose -f .local.compose restart"
abbr --add lcps "docker-compose -f .local.compose ps"

abbr --add j "journalctl"
abbr --add s "systemctl"

# aliases
alias mail="sc $HOME/.password-store/envs/mail.gpg neomutt"
alias slack="weechat"
alias tb="nc termbin.com 9999"
alias pair="docker-compose -f .remote.compose"

# prompt
source "$HOME/.config/fish/prompt.fish"

# set vi mode
fish_vi_key_bindings
