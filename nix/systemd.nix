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
      WorkingDirectory = "/home/placek/.config/dotfiles";
      RemainAfterExit  = "yes";
      ExecStartPre     = "-${pkgs.git}/bin/git clone --recurse-submodules $DOTFILES_URL $DOTFILES_DIR";
      ExecStart        = "${pkgs.stow}/bin/stow --target=$HOME --dir=$DOTFILES_DIR --stow";
      ExecReloadPre    = "-${pkgs.git}/bin/git -C $DOTFILES_DIR pull --ff-only origin master";
      ExecReload       = "${pkgs.stow}/bin/stow --target=$HOME --dir=$DOTFILES_DIR --restow";
      ExecStop         = "${pkgs.stow}/bin/stow --target=$HOME --dir=$DOTFILES_DIR --delete";
    };
    wantedBy = [ "default.target" ];
  };
}
