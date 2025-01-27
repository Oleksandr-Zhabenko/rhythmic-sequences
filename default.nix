{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.mkDerivation {
  pname = "rhythmic-sequences";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ pkgs.haskellPackages.base ];
  description = "Improved library to deal with rhythmicity of short sequences";
  license = pkgs.lib.licenses.mit;
}
