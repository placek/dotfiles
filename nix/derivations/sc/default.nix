{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation rec {
  name = "sc-${version}";
  version = "1.0";

  src = ../../sources/sc;

  buildPhase = "";
  installPhase = ''
    mkdir -p $out/bin
    cp $src/* $out/bin
    chmod +x $out/bin/*
  '';
}
