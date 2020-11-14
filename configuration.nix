{ config, pkgs, ... }:

{
  # hardware
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # system settings
  networking.hostName = "";
  time.timeZone = "Europe/Warsaw";
  i18n.defaultLocale = "pl_PL.UTF-8";

  # system packages are available to every user
  environment.systemPackages = [
    pkgs.git
    pkgs.vim
    pkgs.bash
    pkgs.curl
    pkgs.tmux
    pkgs.tig
    pkgs.silver-searcher
    pkgs.ctags
    pkgs.entr
    pkgs.fish
    pkgs.mutt

    pkgs.haskellPackages.xmonad-contrib
    pkgs.haskellPackages.xmonad-extras
    pkgs.haskellPackages.xmonad
    pkgs.google-chrome
    pkgs.xmobar
    pkgs.rofi
    pkgs.keepassxc
    pkgs.rxvt-unicode
    pkgs.dunst
    pkgs.feh
    pkgs.xdotool
    pkgs.paper-icon-theme
    pkgs.xorg.xkill
    pkgs.xclip
  ];

  users.users = {
    # change this to your name
    placek = {
      isNormalUser = true;
      extraGroups = [ "wheel" ];
      packages = [
      ];
    };
  };

  services.xserver = {
    libinput.enable = true;
    libinput.naturalScrolling = true;
    windowManager.xmonad = {
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib
        haskellPackages.xmonad-extras
        haskellPackages.xmonad
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
      pkgs.iosevka
    ];
    fontconfig = {
      defaultFonts = {
        serif = [ "Ubuntu" ];
        sansSerif = [ "Ubuntu" ];
        monospace = [ "Iosevka Term" ];
      };
    };
    enableDefaultFonts = true;
  };

  networking = {
    interfaces.eth0.useDHCP = true;
    interfaces.wlan0.useDHCP = true;
    wireless.enable = true;
    useDHCP = false;
  };

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda";
  };

  system.stateVersion = "20.09";
}
