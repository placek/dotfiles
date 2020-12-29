{ config, pkgs, ... }:

{
  fonts.enableDefaultFonts                = true;
  fonts.fontconfig.defaultFonts.monospace = [ "Iosevka" ];
  fonts.fontconfig.defaultFonts.sansSerif = [ "Ubuntu" ];
  fonts.fontconfig.defaultFonts.serif     = [ "Ubuntu" ];
  fonts.fonts                             = [ pkgs.iosevka-bin pkgs.ubuntu_font_family ];
  networking.networkmanager.enable        = true;
  powerManagement.enable                  = true;
  programs.gnupg.agent.enable             = true;
  programs.gnupg.agent.enableSSHSupport   = true;
  programs.ssh.startAgent                 = false;
  security.wrappers.slock.source          = "${pkgs.slock.out}/bin/slock";
  sound.enable                            = true;

  services = {
    acpid.enable = true;
    greenclip.enable = true;
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
          background-color = "#858585"
          background-image = ""
          window-color = "#2C3E50"
          border-color = "#3498DB"
          border-width = 4px
          password-background-color = "#2C3E50"
          password-border-width = 0px
        '';
      };
      enable = true;
      layout = "pl";
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

  environment.systemPackages = with pkgs; [
    arandr
    chromium
    dunst
    feh
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras
    libnotify
    moc
    neomutt
    paper-icon-theme
    pinentry-qt
    qutebrowser
    rofi
    rofi-pass
    scrot
    slock
    sxiv
    termite
    xclip
    xdotool
    xmobar
    xmonad-with-packages
    zathura

    (pass.withExtensions (ext: with ext; [pass-otp pass-import]))
    (weechat.override {
      configure = {availablePlugins, ...}: {
        plugins = with availablePlugins; [ python perl ];
        scripts = with pkgs.weechatScripts; [ weechat-notify-send wee-slack ];
        init = ''
          /script install vimode.py
          /set weechat.bar.buflist.size_max 30
          /set weechat.bar.input.items "mode_indicator+[input_prompt]+(away),[input_search], [input_paste],input_text,[vi_buffer]"
          /set weechat.bar.vi_line_numbers.hidden off
          /mouse enable
        '';
      };
    })
  ];
}
