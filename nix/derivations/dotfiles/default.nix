{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation rec {
  name = "dotfiles-${version}";
  version = "dbed8c";

  src = pkgs.fetchFromGitHub {
    owner           = "placek";
    repo            = "dotfiles";
    rev             = "dbed8c6ca96196c1afd2cb6a8917e11aa2727da8";
    sha256          = "0grarlf0qi6mdiq05c87gb1669pgagm5mq826v8qsm1braj5c01h";
  };

  buildInputs = [];
  buildPhase = "";
  installPhase = ''
    mkdir -p $out/shared
    cp -r * $out/shared
  '';
}
