{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# Language ExistentialQuantification #-}
module Simple where

-- Note: to use this module, you must run your program with the -T rtsopt
-- ghc --make -O2 -rtsopts A.hs && ./A +RTS -T
import Easy

import Control.DeepSeq
import System.Directory

import Data.Hashable
import Data.Int
import Data.List
import Data.Monoid
import Data.Proxy
import Data.Word

import System.CPUTime
import System.IO

import GHC.Generics hiding (D)
import GHC.Stats
import GHC.Stack
import System.Mem

import Control.Concurrent
import Easy
import Text.Printf

import Simple.Types

_GCStats_size = 152

{-# INLINE bench #-}
bench :: NFData a => String -> IO a -> ((BenchResult,a) -> Test Sync b) -> Test Sync b
bench nm f w =
    scope nm $ do
        noteScoped "Running benchmark"
        io yield  -- yield for printer to run
        io performGC -- cleanup when we get back
        (before, after, a) <-
            io $ do
                performGC
                !before <- getGCStats
                r <- f
                let !a = force r
                performGC
                !after <- getGCStats
                return (before, after, a)
        cs <- currentScope
        let results = calculate cs before after
        w (results,a)

-- | Given a scope, a list of BenchPreds partially applied to a base, a test to
-- benchmark, and a callback to run if a benchmark is persisted to disk
-- (either first run or all predicates succeed), run an automatic benchmark.
autobench :: NFData a => String -> [BenchResult -> BenchResult -> Bool] -> IO a -> ((BenchResult,a) -> Test Sync b) -> Test Sync b
autobench nm ps f w = do
    bench nm f $ \(new,a) -> do
        mayOld <- revealBench
        case mayOld of
          Nothing -> do
            noteScoped "Autobench writing first results to disk."
            persistBench new
          Just old -> do
            mapM_ (\f -> expect (f old new)) ps
            noteScoped "Autobench writing improved results to disk."
            persistBench new
        w (new,a)

-- rewrite to use directory on ghc and localstorage on ghcjs
-- | Read a list of bench results from a file that were created
-- with the a call to `writeBenches` with the given key.
{-# INLINE readBenches #-}
readBenches :: String -> Test Sync [BenchResult]
readBenches k = do
    cs <- currentScope
    let benchFile = "trivial/benchmarks/" <> show (abs $ hash cs + hash k) <> ".bench"
    exists <- io $ doesFileExist benchFile
    if exists
        then io $ do
            h <- openFile benchFile ReadMode
            oldResults <- read <$> hGetContents h
            oldResults `seq` hClose h
            return oldResults
        else
            return []

-- rewrite to use directory on ghc and localstorage on ghcjs
-- | Persist a list of bench results to a file that will only be
-- readable by calling `readBenches` with the key given to `writeBenches`
-- in this benchmark context.
{-# INLINE writeBenches #-}
writeBenches :: String -> [BenchResult] -> Test Sync ()
writeBenches k brs = do
    cs <- currentScope
    let dir = "trivial/benchmarks/"
    io $ createDirectoryIfMissing True dir
    io $ writeFile (dir <> show (abs $ hash cs + hash k) <> ".bench") (show brs)

-- rewrite to use directory on ghc and localstorage on ghcjs
-- | Read a bench file that was saved in the current scope with `writeBench`.
{-# INLINE revealBench #-}
revealBench :: Test Sync (Maybe BenchResult)
revealBench = do
    cs <- currentScope
    let benchFile = "trivial/benchmarks/" <> show (abs $ hash cs) <> ".bench"
    exists <- io $ doesFileExist benchFile
    if exists
        then io $ do
            h <- openFile benchFile ReadMode
            oldResult <- read <$> hGetContents h
            oldResult `seq` hClose h
            return (Just oldResult)
        else
            return Nothing

-- rewrite to use directory on ghc and localstorage on ghcjs
-- | Persist a bench file that will only be available in this scope with `readBench`.
{-# INLINE persistBench #-}
persistBench :: BenchResult -> Test Sync ()
persistBench br = do
    cs <- currentScope
    let dir = "trivial/benchmarks/"
    io $ createDirectoryIfMissing True dir
    io $ writeFile (dir <> show (abs $ hash cs)) (show br)

{-# INLINE calculate #-}
calculate :: String -> GCStats -> GCStats -> BenchResult
calculate label before after =
    let cpuElapsed = wallSeconds after - wallSeconds before
        cpuTime    = cpuSeconds after - cpuSeconds before

        cpu =
          let elapsed = Duration cpuElapsed
              time    = Duration cpuTime
              factor  = Factor (cpuTime / cpuElapsed)
          in CPU {..}

        gc =
          let e = gcWallSeconds after - gcWallSeconds before
              t = gcCpuSeconds after - gcCpuSeconds before
              elapsed = Duration e
              time    = Duration t
              factor  = Factor (t / e)
              effect  = Percent (e / cpuElapsed)
              burden  = Percent (t / cpuTime)
              bytes   = Bytes $ bytesCopied after - bytesCopied before
              rate    = Throughput $ realToFrac bytes / e
              work    = Throughput $ realToFrac bytes / t
              colls   = Collections $ numGcs after - numGcs before - 1
              live    = Bytes $ currentBytesUsed after - currentBytesUsed before - _GCStats_size
          in GC {..}

        mut =
          let e = mutatorWallSeconds after - mutatorWallSeconds before
              t = mutatorCpuSeconds after - mutatorCpuSeconds before
              elapsed = Duration e
              time    = Duration t
              factor  = Factor (t / e)
              effect  = Percent (e / cpuElapsed)
              burden  = Percent (t / cpuTime)
              bytes   = Bytes $ bytesAllocated after - bytesAllocated before - _GCStats_size
              rate    = Throughput $ realToFrac bytes / e
              work    = Throughput $ realToFrac bytes / t
          in MUT {..}

    in BenchResult {..}

type BenchPred a = a -> BenchResult -> BenchResult -> Bool

data Feature = Garbage | GCs | Clock | Allocs | Mutation
  deriving (Eq,Show,Ord,Read,Enum)

data Predicate
  = forall b. (Real b) => Feature :>  b
  | forall b. (Real b) => Feature :>= b
  | forall b. (Real b) => Feature :=  b
  | forall b. (Real b) => Feature :<= b
  | forall b. (Real b) => Feature :<  b

data SomeImprovingComparable
  = forall a. (Improving a, Similar a, Pretty a) => SIC (BenchResult -> a)

{-# INLINE constrain #-}
constrain :: BenchResult -> BenchResult -> [Predicate] -> Test sync ()
constrain br1 br2 =
  mapM_ $ \p ->
    let pred =
          case p of
            f :> b ->
              (" :>",f,\(SIC s) -> improving (s br1) (s  br2) && not (similar b (s br1) (s br2)))
            f :>= b ->
              (" :>=",f,\(SIC s) -> improving (s br1) (s br2) || similar b (s br1) (s br2))
            f := b ->
              (" >=",f,\(SIC s) -> similar b (s br1) (s br2))
            f :<= b ->
              (" :<=",f,\(SIC s) -> improving (s br2) (s br1) || similar b (s br1) (s br2))
            f :< b ->
              (" :<",f,\(SIC s) -> improving (s br2) (s br1) && not (similar b (s br1) (s br2)))
        selector f =
          case f of
            Garbage  -> SIC $ bytes   .  gc
            GCs      -> SIC $ rate    .  gc
            Clock    -> SIC $ elapsed . cpu
            Allocs   -> SIC $ bytes   . mut
            Mutation -> SIC $ rate    . mut
    in case pred of
        (sc,f,g) -> scope (show f ++ sc) $
          let sel = selector f in
          if g sel then
            ok
          else
            case sel of
              SIC s -> crash $
                intercalate " " [ "Expecting:", pretty (s br1), improvingShow (s br1), pretty (s br2) ]
