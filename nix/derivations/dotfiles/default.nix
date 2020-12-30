{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation rec {
  name = "dotfiles-${version}";
  version = "0937da";

  src = pkgs.fetchFromGitHub {
    owner           = "placek";
    repo            = "dotfiles";
    rev             = "0937da35d5bf303e3f4180955923f6f251a2b923";
    sha256          = "0fkmyxlc76jvmchrcn3hzwakmds2iap5g73b1nzgzkzslymiwkl9";
  };

  buildInputs = [];
  buildPhase = "";
  installPhase = ''
    mkdir -p $out/shared
    cp -r * $out/shared
  '';
}
