{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  trivial = import ../trivial/default.nix {};
  inherit (trivial) t;
  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base, containers, deepseq, directory
      , ghc-prim, hashable, managed, mtl, pretty-show, random, stdenv
      , stm, template-haskell, trivial, unordered-containers
      }:
      mkDerivation {
        pname = "trivial";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          async base containers deepseq directory ghc-prim hashable managed
          mtl pretty-show random stm template-haskell trivial unordered-containers
        ];
        executableHaskellDepends = [
          async base containers deepseq directory ghc-prim hashable managed
          mtl pretty-show random stm template-haskell trivial unordered-containers
        ];
        homepage = "github.com/grumply/champ";
        description = "Simple benchmarking and Easy testing";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
