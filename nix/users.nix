{ config, pkgs, ... }:

{
  users.users = {
    placek = {
      uid = 1000;
      isNormalUser = true;
      description = "Paweł Placzyński";
      extraGroups = [ "wheel" "networkmanager" "docker" ];
      packages = with pkgs; [
        # arduino
        # blender
        # eagle
        # gimp
        # inkscape
        # libreoffice-fresh
        # mplayer
        # musescore
        # shotwell
        # virtualbox
        # vnstat
      ];
      shell = pkgs.fish;
    };
  };
}
