{ config, pkgs, ... }:

{
  # imports
  imports = [
    ./hardware-configuration.nix
  ];

  # common setup
  boot.cleanTmpDir                         = true;
  console.keyMap                           = "pl";
  fonts.enableDefaultFonts                 = true;
  fonts.fontconfig.defaultFonts.monospace  = [ "Iosevka" ];
  fonts.fontconfig.defaultFonts.sansSerif  = [ "Ubuntu" ];
  fonts.fontconfig.defaultFonts.serif      = [ "Ubuntu" ];
  fonts.fonts                              = [ pkgs.iosevka-bin pkgs.ubuntu_font_family ];
  hardware.bluetooth.enable                = true;
  hardware.pulseaudio.enable               = true;
  i18n.defaultLocale                       = "pl_PL.UTF-8";
  networking.firewall.allowPing            = false;
  networking.firewall.allowedTCPPortRanges = [ { from = 3000; to = 3009; } ];
  networking.firewall.enable               = true;
  networking.hostName                      = "vm-nixos";
  networking.networkmanager.enable         = true;
  powerManagement.enable                   = true;
  programs.gnupg.agent.enable              = true;
  programs.gnupg.agent.enableSSHSupport    = true;
  programs.ssh.startAgent                  = false;
  security.wrappers.slock.source           = "${pkgs.slock.out}/bin/slock";
  sound.enable                             = true;
  system.autoUpgrade.allowReboot           = true;
  system.autoUpgrade.channel               = https://nixos.org/channels/nixos-20.09;
  system.autoUpgrade.enable                = true;
  system.stateVersion                      = "20.09";
  time.timeZone                            = "Europe/Warsaw";
  virtualisation.docker.autoPrune.dates    = "daily";
  virtualisation.docker.enable             = true;

  # boot loader
  # boot.loader.systemd-boot.enable = true;
  # boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub = {
    enable  = true;
    version = 2;
    device  = "/dev/sda";
  };

  # software
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
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
    bash
    bat
    ctags
    curl
    docker-compose
    entr
    fd
    fish
    fzf
    git
    gnumake
    inxi
    libnotify
    moc
    ncdu
    neomutt
    openvpn
    paperkey
    pinentry-curses
    rclone
    rsync
    silver-searcher
    stow
    tig
    tmux
    vimHugeX
    wget

    alacritty
    arandr
    chromium
    dunst
    feh
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras
    paper-icon-theme
    pinentry-qt
    qutebrowser
    rofi
    rofi-pass
    scrot
    slock
    xclip
    xdotool
    xmobar
    xmonad-with-packages
    xorg.xkill
    xorg.xmessage
    youtube-dl
  ];

  # users
  users.users = {
    placek = {
      uid = 1000;
      isNormalUser = true;
      description = "Paweł Placzyński";
      extraGroups = [ "wheel" "networkmanager" "docker" ];
      packages = with pkgs; [
        # arduino
        # blender
        # eagle
        # gimp
        # inkscape
        # libreoffice-fresh
        # mplayer
        # musescore
        # shotwell
        # virtualbox
        # vnstat
      ];
      shell = pkgs.fish;
    };
  };

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

  # systemd.user.services.dotfiles = {
  #   description = "Dot files synchronization";
  #   enable = true;
  #   serviceConfig = {
  #     Environment = [
  #       "DOTFILES_URL=git@github.com:placek/dotfiles.git"
  #       "DOTFILES_DIR=.config/dotfiles"
  #     ];
  #     Type             = "oneshot";
  #     WorkingDirectory = "/home/placek/.config/dotfiles";
  #     RemainAfterExit  = "yes";
  #     ExecStartPre     = "-${pkgs.git}/bin/git clone --recurse-submodules $DOTFILES_URL $DOTFILES_DIR";
  #     ExecStart        = "${pkgs.stow}/bin/stow --target=$HOME --dir=$DOTFILES_DIR --stow";
  #     ExecReloadPre    = "-${pkgs.git}/bin/git pull ";
  #     ExecReload       = "${pkgs.stow}/bin/stow --target=$HOME --dir=$DOTFILES_DIR --restow";
  #     ExecStop         = "${pkgs.stow}/bin/stow --target=$HOME --dir=$DOTFILES_DIR --delete";
  #   };
  #   wantedBy = [ "default.target" ];
  # };
}
