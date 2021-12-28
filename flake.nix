{
  description = "Overall configuration files of my own setup.";

  inputs = {
    nixpkgs.url  = "github:NixOS/nixpkgs/9e86f5f7a19db6da2445f07bafa6694b556f9c6d";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs     = import nixpkgs { inherit system; };
      dotfiles = pkgs.stdenv.mkDerivation {
        name         = "dotfiles";
        version      = "test";
        src          = ./.;
        buildInputs  = [ pkgs.haskellPackages.mustache ];
        buildPhase   = ''
          export LANG=C.utf8
          data_file="data.json"
          for file in $(find . -name "*.mustache" -type f); do
            target=$(echo $file | sed 's/\.mustache$//')
            ${pkgs.haskellPackages.mustache}/bin/haskell-mustache $file $data_file > $target
            rm -f $file
          done
        '';
        installPhase = ''
          mkdir -p $out
          cp -r {bin,share,data.json} $out
        '';
      };
    in rec {
      # defaultApp     = flake-utils.lib.mkApp { drv = defaultPackage; };
      defaultPackage = dotfiles;
      devShell       = pkgs.mkShell { buildInputs = [ dotfiles pkgs.shellcheck ]; };
    }
  );
}
