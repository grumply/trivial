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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
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

import System.CPUTime
import System.IO

import GHC.Generics hiding (D)
import GHC.Stats
import GHC.Stack
import System.Mem

import Control.Concurrent
import Easy
import Text.Printf

import Data.Aeson

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

-- | Check if two reals are of similar magnitude given a scaling base `b`.
--
-- In general, as x grows, relative epsilon shrinks as a factor of log_b x.
--
-- `exp 1`, exported as `e`, is a good default base when developing in general. As optimization
-- becomes a focus, shrink `b` towards 1.
--
-- Note: For b < 1, similar b _ _ -> False
--
{-# INLINE similar #-}
similar :: (Real a,Real b) => a -> b -> b -> Bool
similar (realToFrac -> b) (realToFrac -> x) (realToFrac -> y) =
    -- As b ↘ 1, epsilon ↘ 0
    let epsilon = (x :: Double) / (exp 1 * logBase b (x + 1))
        (lo,hi) = (x - epsilon,x + epsilon)
    in lo <= y && y <= hi

{-# INLINE dissimilar #-}
dissimilar :: (Real a, Real b) => a -> b -> b -> Bool
dissimilar b x y = not (similar b x y)

{-# INLINE e #-}
e :: Floating a => a
e = exp 1

class Pretty a where
  pretty :: a -> String

instance Pretty [Char] where
  pretty = id

-- Store duration as microseconds, not fractional seconds!
newtype Duration = Duration { micros :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,Read,Show,ToJSON,FromJSON)
instance Pretty Duration where
    pretty (Duration micros)
        | micros < 1          = printf "%.2fns" (micros * 1000)
        | micros < 1000       = printf "%.2fμs" micros
        | micros < 1000000    = printf "%.2fms" (micros / 1000)
        | micros < 60000000   = printf "%.2fs" (micros / 1000000)
        | micros < 60000000^2 = printf "%dm %ds" (micros / 60000000) ((round micros :: Integer) `mod` 60000000)
        | otherwise           = printf "%dh %dm" (micros / 60000000^2) ((round micros :: Integer) `mod` 60000000^2)

dFromS :: Double -> Duration
dFromS = Duration . (1000000 *)

pattern Minutes :: Double -> Duration
pattern Minutes s <- Duration ((/60000000) -> s) where
  Minutes s = Duration (s * 60000000)

pattern Seconds :: Double -> Duration
pattern Seconds s <- Duration ((/1000000) -> s) where
  Seconds s = Duration (s * 1000000)

pattern Millis :: Double -> Duration
pattern Millis s <- Duration ((/1000) -> s) where
  Millis s = Duration (s * 1000)

pattern Micros :: Double -> Duration
pattern Micros s <- Duration s where
  Micros s = Duration s

pattern Nanos :: Double -> Duration
pattern Nanos s <- Duration ((* 1000) -> s) where
  Nanos s = Duration (s / 1000)

newtype Bytes = Bytes { getBytes :: Int64 }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,ToJSON,FromJSON)
instance Pretty Bytes where
    pretty (Bytes b)
        | b < 2^10  = printf "%d B/s"  b
        | b < 2^20  = printf "%d KB/s" (b `div` 2^10)
        | b < 2^30  = printf "%d MB/s" (b `div` 2^20)
        | otherwise = printf "%d GB/s" (b `div` 2^30)

newtype Throughput = Throughput { getThroughput :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,Read,Show,ToJSON,FromJSON)
instance Pretty Throughput where
    pretty (Throughput t)
        | t < 2^10  = printf "%d B" t
        | t < 2^20  = printf "%d KB" (round t `div` 2^10 :: Int64)
        | t < 2^30  = printf "%d MB" (round t `div` 2^20 :: Int64)
        | t < 2^40  = printf "%d GB" (round t `div` 2^30 :: Int64)
        | otherwise = printf "%d TB" (round t `div` 2^40 :: Int64)

newtype Count = Count { getCount :: Int64 }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,ToJSON,FromJSON)
instance Pretty Count where
  pretty (Count c) = show c

newtype Percent = Percent { getPercent :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,Read,Show,ToJSON,FromJSON)
instance Pretty Percent where
  pretty (Percent p) = printf "%.2f%%" p
percent :: Double -> Percent
percent = Percent . (*100)

newtype Factor = Factor { getFactor :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,Read,Show,ToJSON,FromJSON)
instance Pretty Factor where
  pretty (Factor f) = printf "%.2fx" f

{-# INLINE calculate #-}
calculate :: String -> GCStats -> GCStats -> BenchResult
calculate label before after =
    let cpuElapsed = dFromS $ wallSeconds after - wallSeconds before
        cpuTime    = dFromS $ cpuSeconds after - cpuSeconds before

        cpu =
          let elapsed = cpuElapsed
              time    = cpuTime
              factor  = realToFrac time / realToFrac elapsed
          in CPU {..}

        gc =
          let elapsed = dFromS $ gcWallSeconds after - gcWallSeconds before
              time    = dFromS $ gcCpuSeconds after - gcCpuSeconds before
              factor  = realToFrac time / realToFrac elapsed
              effect  = realToFrac cpuElapsed / realToFrac elapsed
              burden  = realToFrac cpuTime / realToFrac time
              bytes   = Bytes $ bytesCopied after - bytesCopied before
              rate    = realToFrac bytes / realToFrac (elapsed / 1000000)
              work    = realToFrac bytes / realToFrac (time / 1000000)
              colls   = Count $ numGcs after - numGcs before - 1
              live    = Bytes $ currentBytesUsed after - currentBytesUsed before
          in GC {..}

        mut =
          let elapsed = dFromS $ mutatorWallSeconds after - mutatorWallSeconds before
              time    = dFromS $ mutatorCpuSeconds after - mutatorCpuSeconds before
              factor  = realToFrac time / realToFrac elapsed
              effect  = realToFrac cpuElapsed / realToFrac elapsed
              burden  = realToFrac cpuTime / realToFrac time
              bytes   = Bytes $ bytesAllocated after - bytesAllocated before - _GCStats_size - 1296
              rate    = realToFrac bytes / realToFrac (elapsed / 1000000)
              work    = realToFrac bytes / realToFrac (time / 1000000)
          in MUT {..}

    in BenchResult {..}

data Capability
  -- Sum type for the convenience of record punning
  -- record accessors are non-total
  = CPU
        { elapsed      :: {-# UNPACK #-}!Duration   -- ^ wall clock duration
        , time         :: {-# UNPACK #-}!Duration   -- ^ CPU duration
        , factor       :: {-# UNPACK #-}!Factor     -- ^ wall / cpu
        }
   | GC
        { elapsed      :: {-# UNPACK #-}!Duration   -- ^ wall clock duration
        , time         :: {-# UNPACK #-}!Duration   -- ^ CPU duration
        , factor       :: {-# UNPACK #-}!Factor     -- ^ wall / cpu
        , effect       :: {-# UNPACK #-}!Percent    -- ^ impact on wall
        , burden       :: {-# UNPACK #-}!Percent    -- ^ impact on cpu
        , bytes        :: {-# UNPACK #-}!Bytes      -- ^ bytes copied
        , rate         :: {-# UNPACK #-}!Throughput -- ^ bytes copied / wall
        , work         :: {-# UNPACK #-}!Throughput -- ^ bytes copied / cpu
        , colls        :: {-# UNPACK #-}!Count      -- ^ number of GCs
        , live         :: {-# UNPACK #-}!Bytes      -- ^ active bytes after
        }
   | MUT
        { elapsed      :: {-# UNPACK #-}!Duration   -- ^ wall clock duration
        , time         :: {-# UNPACK #-}!Duration   -- ^ CPU duration
        , factor       :: {-# UNPACK #-}!Factor     -- ^ wall / cpu
        , effect       :: {-# UNPACK #-}!Percent    -- ^ impact on wall
        , burden       :: {-# UNPACK #-}!Percent    -- ^ impact on cpu
        , bytes        :: {-# UNPACK #-}!Bytes      -- ^ bytes allocated
        , rate         :: {-# UNPACK #-}!Throughput -- ^ bytes allocated / wall
        , work         :: {-# UNPACK #-}!Throughput -- ^ bytes allocated / cpu
        }
   -- Pretend GC is not parallel for now
   -- | Par
   --      { }
   deriving (Generic, Read, Show, Eq, ToJSON, FromJSON)

data BenchResult = BenchResult
    { label      :: !String
    , cpu        :: {-# UNPACK #-}!Capability
    , mut        :: {-# UNPACK #-}!Capability
    , gc         :: {-# UNPACK #-}!Capability
    } deriving (Generic, Read, Show, Eq, ToJSON, FromJSON)

instance Pretty BenchResult where
    pretty BenchResult {..} =
        unlines
            [ ""
            , header
            , cpuTimeStats
            , mutTimeStats
            , gcTimeStats
            , ""
            , "GC:"       <> p   collections   <> p "MUT:"
            , "  Bytes: " <> p ( bytes    gc ) <> "    Bytes:" <> p ( bytes mut )
            , "  Rate:  " <> p ( rate     gc ) <> "    Rate: " <> p ( rate  mut )
            , "  Work:  " <> p ( work     gc ) <> "    Work: " <> p ( work  mut )
            , "  Live:  " <> p ( live     gc )
            ]
      where
        header = "          Elapsed |"
                 <> "          Time |"
                 <> " Elapsed/Total |"
                 <> "    Time/Total |"
                 <> "       Speedup"

        collections = "(" <> p (colls gc) <> " GCs)"

        cpuTimeStats =
          "CPU:"    <> p ( elapsed cpu )
            <> "  " <> p ( time    cpu )
            <> "  " <> pad ""
            <> "  " <> pad ""
            <> "  " <> p ( factor  cpu )

        mutTimeStats =
          "MUT:"    <> p ( elapsed mut )
            <> "  " <> p ( time    mut )
            <> "  " <> p ( effect  mut )
            <> "  " <> p ( burden  mut )
            <> "  " <> p ( factor  mut )

        gcTimeStats =
          "GC: "    <> p ( elapsed gc )
            <> "  " <> p ( time    gc )
            <> "  " <> p ( effect  gc )
            <> "  " <> p ( burden  gc )
            <> "  " <> p ( factor  gc )

        p :: forall p. Pretty p => p -> String
        p = pad . pretty

        pad :: String -> String
        pad s =
          let l = length s
          in replicate (14 - l) ' ' <> s

type BenchPred a = a -> BenchResult -> BenchResult -> Bool

{-# INLINE faster #-}
faster :: Real a => BenchPred a
faster
  base
  ((elapsed . cpu) -> b1)
  ((elapsed . cpu) -> b2) =
    b1 < b2 && not (similar base b1 b2)

{-# INLINE slower #-}
slower :: Real a => BenchPred a
slower base = flip (faster base)

{-# INLINE fewerAllocations #-}
fewerAllocations :: Real a => BenchPred a
fewerAllocations
  base
  ((bytes . mut) -> b1)
  ((bytes . mut) -> b2) =
    b1 < b2 && not (similar base b1 b2)

{-# INLINE moreAllocations #-}
moreAllocations :: Real a => BenchPred a
moreAllocations base = flip (fewerAllocations base)

{-# INLINE fasterAllocations #-}
fasterAllocations :: Real a => BenchPred a
fasterAllocations
  base
  ((rate . mut) -> b1)
  ((rate . mut) -> b2) =
    b1 > b2 && not (similar base b1 b2)

{-# INLINE slowerAllocations #-}
slowerAllocations :: Real a => BenchPred a
slowerAllocations base = flip (fasterAllocations base)

{-# INLINE fewerGCs #-}
fewerGCs :: Real a => BenchPred a
fewerGCs
  base
  ((colls . gc) -> b1)
  ((colls . gc) -> b2) =
    b1 < b2 && not (similar base b1 b2)

{-# INLINE moreGCs #-}
moreGCs :: Real a => BenchPred a
moreGCs base = flip (fewerGCs base)

{-# INLINE lessGC #-}
lessGC :: Real a => BenchPred a
lessGC
  base
  ((elapsed . gc) -> b1)
  ((elapsed . gc) -> b2) =
    b1 < b2 && not (similar base b1 b2)

{-# INLINE moreGC #-}
moreGC :: Real a => BenchPred a
moreGC base = flip (lessGC base)

{-# INLINE betterUtilization #-}
betterUtilization :: Real a => BenchPred a
betterUtilization base b1 b2 =
  (work $ gc b1) < (work $ gc b2) &&
  (work $ mut b1) < (work $ mut b2) &&
  dissimilar base (work $ gc b1) (work $ gc b2) &&
  dissimilar base (work $ mut b1) (work $ mut b2)

{-# INLNE moreConservative #-}
moreConservative :: Real a => BenchPred a
moreConservative base b1 b2 =
  (bytes $ gc b1) < (bytes $ gc b2) &&
  (bytes $ mut b1) < (bytes $ mut b2) &&
  dissimilar base (bytes $ gc b1) (bytes $ gc b2) &&
  dissimilar base (bytes $ mut b1) (bytes $ mut b2)

{-# INLINE lessConservative #-}
lessConservative :: Real a => BenchPred a
lessConservative base = flip (moreConservative base)

{-# INLINE lessGCThanMutation #-}
lessGCThanMutation :: Real a => a -> BenchResult -> Bool
lessGCThanMutation base b =
  (bytes $ gc b) < (bytes $ mut b) &&
  dissimilar base (bytes $ gc b) (bytes $ mut b)

-- instance Pretty (BenchResult,BenchResult) where
--     pretty (old,new) = do
--         unlines
--             [ ""
--             , header
--             , cpuStats
--             , mutStats
--             , gcStats
--             , ""
--             , "GC:"       <> p   collections   <> p "MUT:"
--             , "  Bytes: " <> p ( bytes    gc ) <> "    Bytes:" <> p ( bytes mut )
--             , "  Rate:  " <> p ( rate     gc ) <> "    Rate: " <> p ( rate  mut )
--             , "  Work:  " <> p ( work     gc ) <> "    Work: " <> p ( work  mut )
--             , "  Live:  " <> p ( live     gc )
--             ]
--       where
--         header = "          Elapsed |"
--                   <> "          Time |"
--                   <> " Elapsed/Total |"
--                   <> "    Time/Total |"
--                   <> "       Speedup"

--         collections = "(" <> p (colls gc) <> " GCs)"

--         cpuTimeStats =
--           "CPU:"    <> p ( elapsed cpu )
--             <> "  " <> p ( time    cpu )
--             <> "  " <> pad ""
--             <> "  " <> pad ""
--             <> "  " <> p ( factor  cpu )

--         mutTimeStats =
--           "MUT:"    <> p ( elapsed mut )
--             <> "  " <> p ( time    mut )
--             <> "  " <> p ( effect  mut )
--             <> "  " <> p ( burden  mut )
--             <> "  " <> p ( factor  mut )

--         gcTimeStats =
--           "GC: "    <> p ( elapsed gc )
--             <> "  " <> p ( time    gc )
--             <> "  " <> p ( effect  gc )
--             <> "  " <> p ( burden  gc )
--             <> "  " <> p ( factor  gc )

--         p = pad . pretty

--         pad :: String -> String
--         pad s =
--           let l = length s
--           in replicate (14 - l) ' ' <> s

