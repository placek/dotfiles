{ config, pkgs, ... }:
let
  secrets = import ../secrets.nix;
in
  {
    boot.cleanTmpDir                         = true;
    console.keyMap                           = "pl";
    i18n.defaultLocale                       = "pl_PL.UTF-8";
    networking.firewall.allowPing            = false;
    networking.firewall.allowedTCPPortRanges = [ { from = 3000; to = 3009; } ];
    networking.firewall.enable               = true;
    nix.gc.automatic                         = true;
    nix.gc.options                           = "--delete-older-than 7d";
    nix.useSandbox                           = true;
    nixpkgs.config.allowUnfree               = true;
    services.cron.enable                     = true;
    services.pcscd.enable                    = true;
    services.printing.enable                 = true;
    services.udev.packages                   = [ pkgs.yubikey-personalization ];
    system.autoUpgrade.allowReboot           = true;
    system.autoUpgrade.channel               = https://nixos.org/channels/nixos-20.09;
    system.autoUpgrade.enable                = true;
    system.stateVersion                      = "20.09";
    time.timeZone                            = "Europe/Warsaw";
    virtualisation.docker.autoPrune.dates    = "daily";
    virtualisation.docker.enable             = true;

    nixpkgs.config.packageOverrides = pkgs: rec {
      dotfiles = pkgs.callPackage ../derivations/dotfiles {};
      projects = pkgs.callPackage ../derivations/projects {};
      sc = pkgs.callPackage ../derivations/sc {};
      todo = pkgs.callPackage ../derivations/todo {};
      wallpapers = pkgs.callPackage ../derivations/wallpapers {};
    };

    users.users = builtins.listToAttrs (map (user:
      {
        name = user.name;
        value = {
          isNormalUser = true;
          extraGroups = [ "wheel" "docker" "networkmanager" "messagebus" "systemd-journal" "disk" "audio" "video" "input" ];
          shell = pkgs.fish;
        } // (removeAttrs user [ "name" ]);
      }
    ) secrets.users);

    environment.systemPackages = with pkgs; [
      bash
      bat
      bc
      cryptsetup
      ctags
      curl
      docker-compose
      entr
      fd
      ffmpeg
      fish
      fzf
      git
      gnumake
      imagemagick7
      inxi
      ncdu
      ngrok
      openvpn
      paperkey
      pinentry-curses
      projects
      rclone
      rsync
      sc
      silver-searcher
      stow
      tig
      tmux
      todo
      vifm-full
      wget
      youtube-dl

      ((vim_configurable.override { python = python3; }).customize {
        name = "vim";
        vimrcConfig.customRC = builtins.readFile ../sources/vim/config;
        vimrcConfig.packages.myVimPackage = with pkgs.vimPlugins; {
          start = [
            YouCompleteMe
            fzf-vim
            fzfWrapper
            nerdtree
            syntastic
            tabular
            ultisnips
            vim-airline
            vim-airline-themes
            vim-fugitive
            vim-gitgutter
          ];
        };
      })
    ];
  }
