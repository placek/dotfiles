{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  # hardware
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # system settings
  networking.hostName = "vm-nixos";
  time.timeZone = "Europe/Warsaw";
  i18n.defaultLocale = "pl_PL.UTF-8";

  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = [
    pkgs.bash
    pkgs.ctags
    pkgs.curl
    pkgs.entr
    pkgs.fish
    pkgs.fzf
    pkgs.git
    pkgs.gnumake
    pkgs.mutt
    pkgs.silver-searcher
    pkgs.tig
    pkgs.tmux
    pkgs.vim

    pkgs.dunst
    pkgs.feh
    pkgs.google-chrome
    pkgs.haskellPackages.xmonad
    pkgs.haskellPackages.xmonad-contrib
    pkgs.haskellPackages.xmonad-extras
    pkgs.keepassxc
    pkgs.networkmanager
    pkgs.paper-icon-theme
    pkgs.rofi
    pkgs.rxvt-unicode
    pkgs.xclip
    pkgs.xdotool
    pkgs.xmobar
    pkgs.xmonad-with-packages
    pkgs.xorg.xkill
  ];

  users.users = {
    placek = {
      isNormalUser = true;
      extraGroups = [ "wheel" ];
      packages = [];
    };
  };

  services.xserver = {
    libinput.enable = true;
    libinput.naturalScrolling = true;
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

  # use `~/.Xresources` to change fonts and other display settings
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
    interfaces.enp0s3.useDHCP = true;
    # interfaces.wlan0.useDHCP = true;
    # wireless.enable = true;
    useDHCP = false;
  };

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda";
  };

  system.stateVersion = "20.09";
}
