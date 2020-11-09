set -U EDITOR vim
set -U PROJECTS_DIR "HOME/Projects"

# remove greetings
set fish_greeting

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

# aliases
alias mail="sc mutt $HOME/.mutt/passwords.gpg"

# colors
source "$HOME/.config/fish/base16-flat.fish"
base16-flat

# budspencer
set -U fish_key_bindings fish_vi_key_bindings
set -U budspencer_nocmdhist cd ls pwd ll c d la
#                     base   result git           insert        visual        c-fg   normal        rs-grn
set budspencer_colors 2C3E50 34495E F1C40F 3498DB 3498DB E74C3C 9B59B6 E74C3C ECF0F1 2ECC71 E74C3C 2ECC71
