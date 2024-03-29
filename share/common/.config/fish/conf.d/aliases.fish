# abbreviations
abbr --add dsp   "docker system prune"
abbr --add dspv  "docker system prune --volumes"
abbr --add dspa  "docker system prune --volumes --all"
abbr --add dcb   "docker-compose build"
abbr --add dcr   "docker-compose run --rm"
abbr --add dcu   "docker-compose up -d"
abbr --add dcl   "docker-compose logs"
abbr --add dcd   "docker-compose down --remove-orphans"
abbr --add dcres "docker-compose restart"
abbr --add dcps  "docker-compose ps"

abbr --add lcb   "docker-compose -f .local.compose build"
abbr --add lcr   "docker-compose -f .local.compose run --rm"
abbr --add lcrt  "docker-compose -f .local.compose run --rm test"
abbr --add lcrw  "docker-compose -f .local.compose run --rm web"
abbr --add lcu   "docker-compose -f .local.compose up -d"
abbr --add lcl   "docker-compose -f .local.compose logs"
abbr --add lcd   "docker-compose -f .local.compose down --remove-orphans"
abbr --add lcres "docker-compose -f .local.compose restart"
abbr --add lcps  "docker-compose -f .local.compose ps"

abbr --add j "journalctl"
abbr --add s "systemctl"
abbr --add p "projects"
abbr --add tt "nc termbin.com 9999"
abbr --add tf "nc oshi.at 7777"

# aliases
alias ls="lsd --icon-theme unicode --hyperlink auto"
alias tree="lsd --icon-theme unicode --tree --hyperlink auto"
alias sshh="TERM=xterm-256color ssh"
alias mail="sc $HOME/.password-store/envs/mail.gpg neomutt"
alias cdt="cd (mktemp -d)"
alias icat="kitty +kitten icat"

function vim
  set -f project (basename (git root 2>/dev/null) 2>/dev/null)

  if test -S "/tmp/$project.socket"
    nvr --remote --servername "/tmp/$project.socket" $argv
  else
    nvim $argv
  end
end

function search --wraps rg; kitty +kitten hyperlinked_grep $argv; end
function gr; git commit -m (curl -Ls http://whatthecommit.com/index.txt); end
