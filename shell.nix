{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7103" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, stdenv, pandoc, shake, cpphs, lmodern }:
      mkDerivation {
        pname = "";
        version = "";
        src = ./.;
        buildDepends = [ pandoc shake ];
        buildTools = [ cpphs lmodern ];
        isLibrary = false;
        isExecutable = false;
        license = stdenv.lib.licenses.unfree;
      };

  drv = pkgs.haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
