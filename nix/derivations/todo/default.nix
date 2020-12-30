{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation rec {
  name = "todo-${version}";
  version = "1.0";

  src = ../../sources/todo;

  buildPhase = "";
  installPhase = ''
    mkdir -p $out/bin
    cp $src/* $out/bin
    chmod +x $out/bin/*
  '';
}
