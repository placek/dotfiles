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
# abbr --add pair "docker-compose -f .remote.compose"
abbr --add tb "nc termbin.com 9999"
abbr --add ns "nix-shell shell.nix --run $SHELL"
abbr --add p "projects"
abbr --add t "todo"

# aliases
alias ls="lsd --icon-theme unicode"
alias mail="sc $HOME/.password-store/envs/mail.gpg neomutt"
alias cdt="cd (mktemp -d)"
alias vi="vim --clean"
alias view="vim -R"

function envup --description 'load dotenv-like files as an env'
  export (grep --no-filename --color=never --invert-match '^#' $argv)
end

function ghc-shell --description 'run nix-shell with ghc and some packages'
  nix-shell -p "ghc.withPackages (pkgs: with pkgs; [ cabal-install $argv ])" --run $SHELL
end

# set vi mode
fish_vi_key_bindings

# colors
set -U fish_color_autosuggestion      brblack
set -U fish_color_cancel              -r
set -U fish_color_command             brgreen
set -U fish_color_comment             brmagenta
set -U fish_color_cwd                 green
set -U fish_color_cwd_root            red
set -U fish_color_end                 brmagenta
set -U fish_color_error               brred
set -U fish_color_escape              brcyan
set -U fish_color_history_current     --bold
set -U fish_color_host                normal
set -U fish_color_match               --background=brblue
set -U fish_color_normal              normal
set -U fish_color_operator            cyan
set -U fish_color_param               brblue
set -U fish_color_quote               yellow
set -U fish_color_redirection         bryellow
set -U fish_color_search_match        'bryellow' '--background=brblack'
set -U fish_color_selection           'white' '--bold' '--background=brblack'
set -U fish_color_status              red
set -U fish_color_user                brgreen
set -U fish_color_valid_path          --underline
set -U fish_pager_color_completion    normal
set -U fish_pager_color_description   yellow
set -U fish_pager_color_prefix        'white' '--bold' '--underline'
set -U fish_pager_color_progress      'brwhite' '--background=cyan'
