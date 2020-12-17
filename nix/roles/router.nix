{ config, pkgs, ... }:

let
  secrets = import ../secrets.nix;
in
  {
    boot.kernel.sysctl."net.ipv4.ip_forward" = 1;

    networking.domain = "local";
    networking.nameservers = [ "127.0.0.1" "8.8.8.8" ];

    networking.firewall = {
      enable = true;
      allowPing = true;
      trustedInterfaces = [ "eno2" "eno3" "eno4" ];
      checkReversePath = false; # https://github.com/NixOS/nixpkgs/issues/10101
      allowedTCPPorts = [
        22    # ssh
        80    # http
        443   # https
        2222  # git
      ];
      allowedUDPPorts = [ ];
    };

    networking.nat = {
      enable = true;
      internalIPs = [ "192.168.2.0/24" "192.168.3.0/24" "192.168.4.0/24" ];
      externalInterface = "eno1";
    };

    networking.interfaces = {
      eno1 = {
        useDHCP = true;
      };

      eno2 = {
        ipAddress = "192.168.2.1";
        prefixLength = 24;
      };

      eno3 = {
        ipAddress = "192.168.3.1";
        prefixLength = 24;
      };

      eno4 = {
        ipAddress = "192.168.4.1";
        prefixLength = 24;
      };
    };

    services.dnsmasq = {
      enable = true;
      servers = [ "8.8.8.8" "8.8.4.4" ];
      extraConfig = ''
        domain=lan
        interface=eno2
        interface=eno3
        interface=eno4
        bind-interfaces
        dhcp-range=192.168.2.10,192.168.2.254,24h
        dhcp-range=192.168.3.10,192.168.3.254,24h
        dhcp-range=192.168.4.10,192.168.4.254,24h
      '';
    };
  }
