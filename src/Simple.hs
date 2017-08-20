{-# LANGUAGE CPP #-}
module Simple (module Export) where

#ifdef __GHCJS__
import Simple.GHCJS as Export
#else
import Simple.GHC as Export
#endif
