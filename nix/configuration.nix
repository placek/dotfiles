{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./common.nix
    ./boot.nix
    ./software.nix
    ./users.nix
    ./services.nix
    ./systemd.nix
  ];
}
