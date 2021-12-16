{
  description = "Full configuration files setup of my own.";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/9e86f5f7a19db6da2445f07bafa6694b556f9c6d;

  outputs = { self, nixpkgs }: {
    defaultPackage.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name        = "dotfiles";
        src          = self;
        buildPhase   = "";
        installPhase = ''
          mkdir -p $out
          cp -r * $out
        '';
      };
  };
}
