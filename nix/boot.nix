{ config, pkgs, ... }:

{
  # boot.loader.systemd-boot.enable = true;
  # boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub = {
    enable  = true;
    version = 2;
    device  = "/dev/sda";
  };
}
