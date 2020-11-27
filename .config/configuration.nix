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
    openvpn
    rclone
    rsync
    silver-searcher
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
    keepassxc
    paper-icon-theme
    pinentry
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
      dpi = 144;
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

  fonts = {
    fonts = [
      pkgs.corefonts
      pkgs.iosevka-bin
      pkgs.ubuntu_font_family
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
