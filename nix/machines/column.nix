{ config, lib, pkgs, ... }:

{
  imports = [
    ../roles/common.nix
    # ../roles/router.nix
  ];

  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.enable      = true;
  networking.hostName                  = "column";
  services.sshd.enable                 = true;
  services.sshd.passwordAuthentication = false;

  # remove after new internet arrive
  networking.wireless.enable = true;
  networking.wireless.networks = {
    "placki-play" = {
      pskRaw = "7ac564da03c01a69bd1f1465412038365e5b29f41f9d584aa8978d22110b5f6f";
    };
  };
}
