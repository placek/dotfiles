set -gx EDITOR vim
set -gx PROJECTS_DIR "$HOME/Projects"
set -gx DOTFILES_DIR "$HOME/.config/dotfiles"

# pfetch greetings
function fish_greeting
  inxi
end

# local binaries
if [ -d "$HOME/.local/bin" ]
  set PATH $PATH "$HOME/.local/bin"
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
alias mail="neomutt"
alias slack="weechat"
alias tb="nc termbin.com 9999"
alias dots="stow --dir=$DOTFILES_DIR/home --target=$HOME --verbose"

# functions
function rebuild-nix --description "copy configuration and rebuild system"
  sudo cp $DOTFILES_DIR/nix/*.nix /etc/nixos/
  sudo nixos-rebuild switch
end

# colors
source "$HOME/.config/fish/base16-flat.fish"
base16-flat

# prompt
source "$HOME/.config/fish/prompt.fish"

# secrets
#   gpg -r your.email@example.com -e .env
for file in (find $HOME -maxdepth 1 -type f | grep .env.gpg)
  gpg -dq $file 2> /dev/null | source
end

# set vi mode
fish_vi_key_bindings
