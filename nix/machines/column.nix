{ config, lib, pkgs, ... }:

{
  imports = [
    ../roles/common.nix
    ../services/dotfiles.nix
  ];

  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.enable      = true;
  networking.hostName                  = "column";
  services.sshd.enable                 = true;
  services.sshd.passwordAuthentication = false;
}
