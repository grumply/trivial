{ mkDerivation, aeson, async, base, containers, deepseq, directory
, ghc-prim, hashable, managed, mtl, pretty-show, random, stdenv
, stm, template-haskell, unordered-containers, vector, ghcjs-base
, ghc
}:
mkDerivation {
  pname = "trivial";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base containers deepseq directory ghc-prim hashable managed
    mtl pretty-show random stm template-haskell unordered-containers vector
    ghc ghcjs-base
  ];
  homepage = "github.com/grumply/champ";
  description = "Simple benchmarking and Easy testing";
  license = stdenv.lib.licenses.bsd3;
}
