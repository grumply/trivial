{ mkDerivation, async, base, containers, deepseq, directory
, ghc-prim, hashable, managed, mtl, pretty-show, random, stdenv
, stm, template-haskell
}:
mkDerivation {
  pname = "trivial";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base containers deepseq directory ghc-prim hashable managed
    mtl pretty-show random stm template-haskell
  ];
  executableHaskellDepends = [
    async base containers deepseq directory ghc-prim hashable managed
    mtl pretty-show random stm template-haskell
  ];
  homepage = "github.com/grumply/champ";
  description = "Simple benchmarking and Easy testing";
  license = stdenv.lib.licenses.bsd3;
}
