set -gx EDITOR vim
set -gx PROJECTS_DIR "$HOME/Projects"
set -gx DOTFILES_DIR "$HOME/.config/dotfiles"

# pfetch greetings
function fish_greeting
  pfetch
end

# local binaries
if [ -d "$HOME/.local/bin" ]
  set PATH $PATH "$HOME/.local/bin"
end

# unbind Ctrl-V
bind -e \cv

# abbreviations
abbr --add be "bundle exec"
abbr --add bi "bundle install"
abbr --add dcb "docker-compose build"
abbr --add dcr "docker-compose run --rm"
abbr --add dcu "docker-compose up -d"
abbr --add dcl "docker-compose logs"
abbr --add dcd "docker-compose down"
abbr --add dsp "docker system prune"
abbr --add dspv "docker system prune --volumes"
abbr --add dcres "docker-compose restart"
abbr --add dcps "docker-compose ps"
abbr --add j "journalctl"
abbr --add s "systemctl"

# aliases
alias mail="sc $HOME/.mutt/passwords.gpg neomutt"
alias tb="nc termbin.com 9999"
alias dots="git --git-dir=$DOTFILES_DIR --work-tree=$HOME"

# functions
function rebuild-nix --description "copy configuration and rebuild system"
  sudo cp $HOME/.config/configuration.nix /etc/nixos/configuration.nix
  sudo nixos-rebuild switch
end

# colors
source "$HOME/.config/fish/base16-flat.fish"
base16-flat

# prompt
source "$HOME/.config/fish/prompt.fish"

# secrets
[ -f "$HOME/.env.gpg" ] && gpg -dq "$HOME/.env.gpg" 2> /dev/null | source
