{ config, pkgs, ... }:

{
  boot.cleanTmpDir                         = true;
  console.keyMap                           = "pl";
  fonts.enableDefaultFonts                 = true;
  fonts.fontconfig.defaultFonts.monospace  = [ "Iosevka" ];
  fonts.fontconfig.defaultFonts.sansSerif  = [ "Ubuntu" ];
  fonts.fontconfig.defaultFonts.serif      = [ "Ubuntu" ];
  fonts.fonts                              = [ pkgs.iosevka-bin pkgs.ubuntu_font_family ];
  hardware.bluetooth.enable                = true;
  hardware.pulseaudio.enable               = true;
  i18n.defaultLocale                       = "pl_PL.UTF-8";
  networking.firewall.allowPing            = false;
  networking.firewall.allowedTCPPortRanges = [ { from = 3000; to = 3009; } ];
  networking.firewall.enable               = true;
  networking.hostName                      = "vm-nixos";
  networking.networkmanager.enable         = true;
  nix.gc.automatic                         = true;
  nix.gc.options                           = "--delete-older-than 7d";
  powerManagement.enable                   = true;
  programs.gnupg.agent.enable              = true;
  programs.gnupg.agent.enableSSHSupport    = true;
  programs.ssh.startAgent                  = false;
  security.wrappers.slock.source           = "${pkgs.slock.out}/bin/slock";
  sound.enable                             = true;
  system.autoUpgrade.allowReboot           = true;
  system.autoUpgrade.channel               = https://nixos.org/channels/nixos-20.09;
  system.autoUpgrade.enable                = true;
  system.stateVersion                      = "20.09";
  time.timeZone                            = "Europe/Warsaw";
  virtualisation.docker.autoPrune.dates    = "daily";
  virtualisation.docker.enable             = true;
}
