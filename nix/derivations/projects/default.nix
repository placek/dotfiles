{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation rec {
  name = "projects-${version}";
  version = "1.0";

  src = ../../sources/projects;

  buildPhase = "";
  installPhase = ''
    mkdir -p $out/bin
    cp $src/binary $out/bin/projects
    chmod +x $out/bin/projects
  '';
}
