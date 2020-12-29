{ config, lib, pkgs, ... }:

{
  imports = [
    ../roles/common.nix
    ../roles/workstation.nix
    ../services/dotfiles.nix
  ];

  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.enable      = true;
  hardware.bluetooth.enable            = true;
  hardware.facetimehd.enable           = true;
  hardware.opengl.extraPackages        = [ pkgs.vaapiIntel ];
  hardware.pulseaudio.enable           = true;
  networking.hostName                  = "lambda";
  powerManagement.enable               = true;
  programs.light.enable                = true;

  services.xserver.synaptics.enable            = true;
  services.xserver.synaptics.dev               = "/dev/input/event7";
  services.xserver.synaptics.tapButtons        = false;
  services.xserver.synaptics.buttonsMap        = [ 1 3 2 ];
  services.xserver.synaptics.twoFingerScroll   = true;
  services.xserver.synaptics.palmDetect        = false;
  services.xserver.synaptics.accelFactor       = "0.001";
  services.xserver.synaptics.additionalOptions = ''
    Option "SHMConfig" "on"
    Option "VertScrollDelta" "-100"
    Option "HorizScrollDelta" "-100"
    Option "Resolution" "370"
  '';
  services.mbpfan = {
    enable = true;
    lowTemp = 61;
    highTemp = 64;
    maxTemp = 84;
  };

  users.users.placek.packages          = with pkgs; [ arduino blender eagle gimp inkscape libreoffice-fresh mplayer musescore shotwell virtualbox vnstat ];
}
