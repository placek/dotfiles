{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  # hardware
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # system settings
  i18n.defaultLocale = "pl_PL.UTF-8";
  networking.hostName = "vm-nixos";
  networking.networkmanager.enable = true;
  time.timeZone = "Europe/Warsaw";
  virtualisation.docker.enable = true;

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
    mutt
    silver-searcher
    tig
    tmux
    vim

    dunst
    feh
    google-chrome
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras
    keepassxc
    paper-icon-theme
    rofi
    rxvt-unicode
    scrot
    xclip
    xdotool
    xmobar
    xmonad-with-packages
    xorg.xkill
  ];

  users.users = {
    placek = {
      isNormalUser = true;
      extraGroups = [ "wheel" "networkmanager" "docker" ];
      packages = [ ];
    };
  };

  services.xserver = {
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
    # wireless.enable = true;
  };

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda";
  };

  system.stateVersion = "20.09";
}
