{ compiler ? "ghc801" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = new: old: rec {

              trivial =
                new.callPackage ./trivial.nix { };

            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { trivial = pkgs.haskell.packages.${compiler}.trivial;
  }

