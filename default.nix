{ mkDerivation, ghc, ghcjs-base, aeson, async, base, deepseq, directory, ghc-prim
, hashable, mtl, random, stdenv, stm, text, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "trivial";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base deepseq directory ghc-prim hashable mtl random stm
    text transformers unordered-containers vector
  ] ++ (if ghc.isGhcjs or false then [ ghcjs-base ] else []);
  homepage = "github.com/grumply/trivial";
  description = "Simple benchmarking and Easy testing for GHC and GHCJS";
  license = stdenv.lib.licenses.bsd3;
}
