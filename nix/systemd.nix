{ config, pkgs, ... }:

{
  systemd.user.services.dotfiles = {
    description = "Dot files synchronization";
    enable = true;
    serviceConfig = {
      Environment = [
        "DOTFILES_URL=git@github.com:placek/dotfiles.git"
        "DOTFILES_DIR=.config/dotfiles"
      ];
      Type             = "oneshot";
      RemainAfterExit  = "yes";
      ExecStartPre     = "-${pkgs.git}/bin/git clone --recurse-submodules $DOTFILES_URL $HOME/$DOTFILES_DIR";
      ExecReloadPre    = "-${pkgs.git}/bin/git -C $HOME/$DOTFILES_DIR pull --ff-only origin master";
      ExecStart        = "${pkgs.bash}/bin/bash -c 'ls -1 $DOTFILES_DIR/home | xargs ${pkgs.stow}/bin/stow --target=$HOME --dir=$HOME/$DOTFILES_DIR --stow'";
      ExecReload       = "${pkgs.bash}/bin/bash -c 'ls -1 $DOTFILES_DIR/home | xargs ${pkgs.stow}/bin/stow --target=$HOME --dir=$HOME/$DOTFILES_DIR --restow'";
      ExecStop         = "${pkgs.bash}/bin/bash -c 'ls -1 $DOTFILES_DIR/home | xargs ${pkgs.stow}/bin/stow --target=$HOME --dir=$HOME/$DOTFILES_DIR --delete'";
    };
    wantedBy = [ "default.target" ];
  };
}
