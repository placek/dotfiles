{ config, lib, pkgs, ... }:

{
  imports = [
    ../roles/common.nix
    ../roles/workstation.nix
  ];

  boot.loader.efi.canTouchEfiVariables       = true;
  boot.loader.systemd-boot.enable            = true;
  hardware.bluetooth.enable                  = true;
  hardware.facetimehd.enable                 = true;
  hardware.opengl.extraPackages              = [ pkgs.vaapiIntel ];
  hardware.pulseaudio.enable                 = true;
  networking.hostName                        = "lambda";
  powerManagement.enable                     = true;
  programs.light.enable                      = true;
  services.xserver.libinput.enable           = true;
  services.xserver.libinput.naturalScrolling = true;
  services.xserver.libinput.scrollMethod     = "twofinger";
  services.mbpfan.enable                     = true;
  services.mbpfan.lowTemp                    = 61;
  services.mbpfan.highTemp                   = 64;
  services.mbpfan.maxTemp                    = 84;

  users.users.placek.packages = with pkgs; [
    arduino
    blender
    dotfiles
    eagle
    fusuma
    gimp
    inkscape
    libreoffice-fresh
    mplayer
    musescore
    shotwell
    virtualbox
    vnstat
  ];
}
