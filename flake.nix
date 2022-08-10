{
  description = "Overall configuration files of my own setup.";

  inputs = {
    nixpkgs.url     = "github:NixOS/nixpkgs/9e86f5f7a19db6da2445f07bafa6694b556f9c6d";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs     = import nixpkgs { inherit system; };
      dotfiles = pkgs.stdenv.mkDerivation {
        name         = "dotfiles";
        version      = "test";
        src          = ./.;
        installPhase = ''
          mkdir -p $out
          cp -r {bin,share} $out
        '';
      };
    in rec {
      defaultPackage = dotfiles;
      devShell       = pkgs.mkShell {
        buildInputs = [ dotfiles pkgs.nodePackages.bash-language-server pkgs.rnix-lsp ];
      };
    }
  );
}
