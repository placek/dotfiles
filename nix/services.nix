{ config, pkgs, ... }:

{
  services = {
    acpid.enable = true;
    cron.enable = true;
    greenclip.enable = true;
    printing.enable = true;
    xserver = {
      displayManager.defaultSession = "none+xmonad";
      displayManager.lightdm = {
        enable = true;
        greeters.mini.enable = true;
        greeters.mini.user = "placek";
        greeters.mini.extraConfig = ''
          [greeter]
          show-password-label = false
          invalid-password-text = nope!
          show-input-cursor = false
          password-alignment = left
          password-input-width = 12
          [greeter-theme]
          font = "Iosevka"
          font-weight = normal
          error-color = "#F5F5F5"
          password-color = "#F5F5F5"
          background-color = "#5F5F5F"
          window-color = "#2C3E50"
          border-color = "#3498DB"
          border-width = 4px
          password-background-color = "#2C3E50"
          password-border-width = 0px
        '';
      };
      enable = true;
      layout = "pl";
      libinput.enable = true;
      windowManager.xmonad = {
        enableContribAndExtras = true;
        haskellPackages = pkgs.haskell.packages.ghc865;
        extraPackages = haskellPackages: with haskellPackages; [
          alsa-core
          alsa-mixer
          xmonad
          xmonad-contrib
          xmonad-extras
        ];
        enable = true;
      };
      xautolock = {
        enable = true;
        enableNotifier = true;
        locker = "${pkgs.slock}/bin/slock";
        notifier = "${pkgs.libnotify}/bin/notify-send 'Locking in 10 seconds'";
      };
    };
  };
}
