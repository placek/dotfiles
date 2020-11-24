{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  boot.cleanTmpDir = true;
  console.keyMap = "pl";
  hardware.bluetooth.enable = true;
  hardware.pulseaudio.enable = true;
  i18n.defaultLocale = "pl_PL.UTF-8";
  powerManagement.enable = true;
  security.wrappers.slock.source = "${pkgs.slock.out}/bin/slock";
  sound.enable = true;
  time.timeZone = "Europe/Warsaw";
  virtualisation.docker.enable = true;

  programs = {
    ssh.startAgent = false;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
  };

  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    bash
    ctags
    curl
    docker-compose
    entr
    fish
    fzf
    git
    gnumake
    libnotify
    moc
    neomutt
    ncdu
    passExtensions.pass-otp
    (pass.withExtensions (ext: with ext; [pass-otp pass-import]))
    pfetch
    pinentry-curses
    rclone
    rsync
    silver-searcher
    tig
    tmux
    vim

    chromium
    dunst
    feh
    haskellPackages.greenclip
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras
    keepassxc
    paper-icon-theme
    pinentry-qt
    qutebrowser
    rofi
    rofi-pass
    rxvt-unicode
    scrot
    slock
    termonad-with-packages
    xclip
    xdotool
    xmobar
    xmonad-with-packages
    xorg.xkill
    youtube-dl
  ];

  users.users = {
    placek = {
      uid = 1000;
      isNormalUser = true;
      description = "Paweł Placzyński";
      extraGroups = [ "wheel" "networkmanager" "docker" ];
      packages = [];
      shell = pkgs.fish;
    };
  };

  services.xserver = {
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
    displayManager.defaultSession = "none+xmonad";
    displayManager.lightdm.enable = true;
    enable = true;
  };

  fonts = {
    fonts = [
      pkgs.ubuntu_font_family
      pkgs.iosevka-bin
    ];
    fontconfig = {
      defaultFonts = {
        serif = [ "Ubuntu" ];
        sansSerif = [ "Ubuntu" ];
        monospace = [ "Iosevka" ];
      };
    };
    enableDefaultFonts = true;
  };

  networking = {
    firewall.allowPing    = false;
    firewall.enable       = true;
    firewall.allowedTCPPortRanges = [
      { from = 3000; to = 3009; }
    ];
    hostName              = "vm-nixos";
    networkmanager.enable = true;
    # wireless.enable     = true;
    # wireless.networks = {
    #   placki = {
    #     ssid = "placki";
    #     pskRaw = "3e77b06e92098e903627d0d9cd2222a77f54bc09b14bfcf780b01bdb0c165e39";
    #   };
    #   plackiplay = {
    #     ssid = "placki-play";
    #     pskRaw = "7ac564da03c01a69bd1f1465412038365e5b29f41f9d584aa8978d22110b5f6f";
    #   };
    # };
  };

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda";
  };

  systemd.user.services.dotfiles = {
    description = "Dot files synchronization";
    enable = true;
    serviceConfig = {
      Type = "oneshot";
      Environment = [
        "DOTFILES_URL=https://github.com/placek/dotfiles.git"
        "DOTFILES_DIR=.config/dotfiles"
        "XARGS=${pkgs.busybox}/bin/xargs"
        "GREP=${pkgs.busybox}/bin/grep"
        "GIT=${pkgs.git}/bin/git"
      ];
      WorkingDirectory = "$HOME/$DOTFILES_DIR";
      RemainAfterExit  = "yes";
      ExecStartPre = "!${pkgs.git}/bin/git clone --recurse-submodules $DOTFILES_URL .";
      ExecStart    = "${pkgs.gnumake}/bin/make install";
      ExecReload   = "${pkgs.git}/bin/git reset --hard && ${pkgs.git}/bin/git pull --ff-only origin master && ${pkgs.gnumake}/bin/make install";
      ExecStop     = "${pkgs.gnumake}/bin/make clean";
    };
    wantedBy = [ "default.target" ];
  };

  system = {
    autoUpgrade = {
      allowReboot = true;
      enable = true;
      channel = https://nixos.org/channels/nixos-20.09;
    };
    stateVersion = "20.09";
  };
}
