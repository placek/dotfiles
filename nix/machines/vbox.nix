{ config, lib, pkgs, ... }:

{
  imports = [
    ../roles/common.nix
    ../roles/workstation.nix
    ../services/dotfiles.nix
  ];

  boot.loader.grub = {
    enable  = true;
    version = 2;
    device  = "/dev/sda";
  };
  networking.hostName = "vbox";
}
