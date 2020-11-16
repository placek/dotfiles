{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  # hardware
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # system settings
  console.keyMap = "pl";
  i18n.defaultLocale = "pl_PL.UTF-8";
  networking.hostName = "vm-nixos";
  networking.networkmanager.enable = true;
  time.timeZone = "Europe/Warsaw";
  security.wrappers.slock.source = "${pkgs.slock.out}/bin/slock";
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
    slock
    xclip
    xdotool
    xmobar
    xmonad-with-packages
    xorg.xkill
  ];

  users.users = {
    placek = {
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
    # wireless.enable = true;
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
        "REPO_URL=https://github.com/placek/dotfiles.git"
        "DOTFILES_DIR=.config/dotfiles"
      ];
      RemainAfterExit = "yes";
      ExecStartPre = "${pkgs.bash}/bin/bash -c '[ -d $HOME/$DOTFILES_DIR ] || ${pkgs.git}/bin/git clone --recurse-submodules $REPO_URL $HOME/$DOTFILES_DIR'";
      ExecStart    = "${pkgs.bash}/bin/bash -c 'cd $HOME/$DOTFILES_DIR && ${pkgs.gnumake}/bin/make install'";
      ExecReload   = "${pkgs.bash}/bin/bash -c 'cd $HOME/$DOTFILES_DIR && ${pkgs.git}/bin/git pull --ff origin master && ${pkgs.gnumake}/bin/make install'";
      ExecStop     = "${pkgs.bash}/bin/bash -c 'cd $HOME/$DOTFILES_DIR && ${pkgs.gnumake}/bin/make clean'";
    };
    wantedBy = [ "default.target" ];
  };

  system.stateVersion = "20.09";
}
