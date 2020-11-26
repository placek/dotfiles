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

  virtualisation.docker = {
    autoPrune.dates = "daily";
    enable = true;
  };

  programs = {
    ssh.startAgent = false;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
  };

  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    (pass.withExtensions (ext: with ext; [pass-otp pass-import]))
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
    passExtensions.pass-otp
    pinentry-curses
    rclone
    rsync
    silver-searcher
    tig
    tmux
    vim
    wget

    alacritty
    arandr
    chromium
    dunst
    feh
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras
    keepassxc
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

  services = {
    acpid.enable = true;
    cron.enable = true;
    greenclip.enable = true;
    printing.enable = true;
    xserver = {
      displayManager.defaultSession = "none+xmonad";
      displayManager.lightdm.enable = true;
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

  system = {
    autoUpgrade = {
      allowReboot = true;
      enable = true;
      channel = https://nixos.org/channels/nixos-20.09;
    };
    stateVersion = "20.09";
  };
}
