{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation rec {
  name = "wallpapers-${version}";
  version = "a341d5";

  src = pkgs.fetchGit {
    url    = "https://gitlab.com/dwt1/wallpapers.git";
    rev    = "a341d5b24ef8abbd71b44a0dacecdb301fb78eea";
    sha256 = "1bd35dkhwzw3xjbbx9ignnr81wy841krk8cqxnjqsc4sgi9ic360";
  };

  buildInputs = [];
  buildPhase = "";
  installPhase = ''
    mkdir -p $out/shared
    cp -r *.jpg $out/shared
  '';
}
