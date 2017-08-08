{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Simple where

-- Note to use this module, you must compile your program with the -T rtsopt
-- ghc --make -O2 -rtsopts A.hs && ./A +RTS -T
import Easy

import Control.DeepSeq
import System.Directory

import Data.Hashable
import Data.Int
import Data.List
import Data.Monoid

import Text.Show.Pretty

import System.CPUTime

import GHC.Stats
import GHC.Generics hiding (D)
import System.Mem

import Text.Printf
import Control.Concurrent
import Easy

_GCStats_size = 152

{-# INLINE bench #-}
bench :: NFData a => String -> IO a -> Test Sync (BenchResults,a)
bench nm f =
    scope nm $ do
        note "Running"
        (before,after,a) <- io $ do
          -- Force a collection cycle before getting
          -- the start time and start GC stats
          performGC
          yield
          !before <- getGCStats
          r <- f
          let !a = force r
          performGC
          !after <- getGCStats
          return (before,after,a)
        cs <- currentScope
        let results = calculate cs before after
        noteResults results
        completed
        io yield
        return (results,a)

{-# INLINE noteResults #-}
noteResults :: BenchResults -> Test Sync ()
noteResults results = do
    cs <- currentScope
    let benchfile = "trivial/benchmarks/" <> show (abs $ hash cs) <> ".bench"
    oldResultsExist <- io $ doesFileExist benchfile
    if oldResultsExist
        then do
            !oldResults <- io $ read <$> readFile benchfile
            io $ writeFile benchfile (show (results : oldResults))
            noteDiff results oldResults
        else do
            writeSynopsis results
            io $ createDirectoryIfMissing True "trivial/benchmarks/"
            io $ writeFile benchfile (show [results])

{-# INLINE calculate #-}
calculate :: String -> GCStats -> GCStats -> BenchResults
calculate label before after =
    let
        duration                   = D $ max 0 (wallSeconds after)        - max 0 (wallSeconds before)
        gc_duration                = D $ max 0 (gcWallSeconds after)      - max 0 (gcWallSeconds before)
        mutation_duration          = D $ max 0 (mutatorWallSeconds after) - max 0 (mutatorWallSeconds before)
        active_bytes               = max 0 (currentBytesUsed after)   - max 0 (currentBytesUsed before)
        cpu_work                   = D $ max 0 (cpuSeconds after)         - max 0 (cpuSeconds before)
        gc_work                    = D $ max 0 (gcCpuSeconds after)       - max 0 (gcCpuSeconds before)
        mutation_work              = D $ max 0 (mutatorCpuSeconds after)  - max 0 (mutatorCpuSeconds before)
        allocations                = max 0 (bytesAllocated after)     - max 0 (bytesAllocated before) - _GCStats_size - 1296
        allocation_rate            = D $ realToFrac allocations       / fromD duration
        allocation_work            = D $ realToFrac allocations       / fromD cpu_work
        collections                = numGcs after                 - numGcs before - 1
        collection_rate            = D $ realToFrac copied_bytes      / fromD duration
        collection_work            = D $ realToFrac copied_bytes      / fromD gc_work
        copied_bytes               = bytesCopied after            - bytesCopied before
        cpu_factor                 = D $ realToFrac (fromD cpu_work)          / fromD duration
        gc_factor                  = D $ realToFrac (fromD gc_work)           / fromD gc_duration
        gc_work_relative           = D $ realToFrac (fromD gc_work)           / fromD cpu_work
        gc_duration_relative       = D $ realToFrac (fromD gc_duration)       / fromD duration
        mutation_factor            = D $ realToFrac (fromD mutation_work)     / fromD mutation_duration
        mutation_work_relative     = D $ realToFrac (fromD mutation_work)     / fromD cpu_work
        mutation_duration_relative = D $ realToFrac (fromD mutation_duration) / fromD duration

    in
      BenchResults {..}

newtype D = D { fromD :: Double } deriving (Eq)
instance Show D where
  show (D d) = printf "%.6f" d
instance Read D where
  readsPrec i s = map (\(r,s) -> (D r,s)) (readsPrec i s)

data BenchResults = BenchResults
    { label :: !String
    , duration :: {-# UNPACK #-}!D        -- wall-clock time
    , gc_duration :: {-# UNPACK #-}!D      -- wall-clock time
    , mutation_duration :: {-# UNPACK #-}!D -- wall-clock time
    , allocations :: {-# UNPACK #-}!Int64      -- bytes allocated
    , allocation_rate :: {-# UNPACK #-}!D
    , allocation_work :: {-# UNPACK #-}!D
    , collections :: {-# UNPACK #-}!Int64      -- gc collection cycles
    , collection_rate :: {-# UNPACK #-}!D
    , collection_work :: {-# UNPACK #-}!D
    , copied_bytes :: {-# UNPACK #-}!Int64           -- bytes copied by gc
    , active_bytes :: {-# UNPACK #-}!Int64           -- bytes created and not GCd during run
    , cpu_work :: {-# UNPACK #-}!D            -- cpu time
    , cpu_factor :: {-# UNPACK #-}!D
    , gc_work :: {-# UNPACK #-}!D          -- cpu time
    , gc_factor :: {-# UNPACK #-}!D
    , gc_work_relative :: {-# UNPACK #-}!D
    , gc_duration_relative :: {-# UNPACK #-}!D
    , mutation_work :: {-# UNPACK #-}!D     -- cpu time
    , mutation_factor :: {-# UNPACK #-}!D
    , mutation_work_relative :: {-# UNPACK #-}!D
    , mutation_duration_relative :: {-# UNPACK #-}!D
    } deriving (Generic, Read, Show, Eq)

{-# INLINE noteDiff #-}
noteDiff :: BenchResults -> [BenchResults] -> Test sync ()
noteDiff old new = do
  note ""

{-# INLINE writeSynopsis #-}
writeSynopsis :: BenchResults -> Test sync ()
writeSynopsis results = do
  noteScoped ": First run results..."
  note (ppShow results)

