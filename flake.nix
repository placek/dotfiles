{
  description = "Overall configuration files of my own setup.";

  inputs = {
    nixpkgs.url  = "github:NixOS/nixpkgs/9e86f5f7a19db6da2445f07bafa6694b556f9c6d";
  };

  outputs = { self, nixpkgs }: {
    defaultPackage.x86_64-linux = let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
    in pkgs.stdenv.mkDerivation {
      name         = "dotfiles";
      version      = "test";
      src          = ./.;
      buildPhase   = "";
      installPhase = ''
        mkdir -p $out
        cp -r {bin,share,data.json} $out
      '';
    };
  };
}
