{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./machines/vbox.nix # or any other machine
  ];
}
