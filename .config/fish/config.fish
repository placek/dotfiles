set -U EDITOR vim
set -U PROJECTS_DIR "HOME/Projects"

# local binaries
if [ -d "$HOME/.local/bin" ]
  set PATH $PATH "$HOME/.local/bin"
end

# unbind Ctrl-V
bind -a -e \cv

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
