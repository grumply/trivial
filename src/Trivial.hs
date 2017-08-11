{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Trivial (module Export, module Trivial) where

import Easy as Export
import Simple as Export hiding (copyRate)
import qualified Simple

import Simple.Internal
import Simple.Internal.Count

import Data.Int

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Data.Monoid
import GHC.Stats
import System.Mem
import System.CPUTime

import Debug.Trace

withEnv :: (NFData env) => String -> Test Sync env -> (env -> Test Sync ()) -> Test Sync ()
withEnv nm mkenv f = do
  e <- mkenv
  e `deepseq` f e

data BenchResult = BenchResult
  { runs :: SomeCount
  , dur :: CPUTime
  , cpu :: CPUTime
  , mut :: CPUTime
  , gc  :: CPUTime
  , alloc :: Allocated
  , allocRate :: AllocationRate
  , copy :: Copied
  , copyRate :: CopyRate
  , coll :: Collections
  } deriving (Show,Read,Eq)
instance Pretty BenchResult where
    pretty BenchResult {..} =
        unlines
            [ ""
            , divider
            , header2
            , divider
            , cpuTimeStats
            , mutTimeStats
            , gcTimeStats
            , ""
            , "Runs:       " <> pad 11 (pretty runs)
            , "Duration:   " <> pad 11 (pretty dur)
            , "Collections:" <> pad 11 (pretty coll)
            ]
      where
        header2 = "                Time           |       Space |    Throughput"
        divider = "     -------------------------------------------------------"

        cpuTimeStats =
          "CPU:"    <> p cpu

        mutTimeStats =
          "MUT:"    <> p mut
            <> "  " <> p (mkPercent mut cpu)
            <> "  " <> p alloc
            <> "  " <> pad 14 (pretty (DataRate alloc mut :: SomeDataRate))

        gcTimeStats =
          "GC: "    <> p gc
            <> "  " <> p (mkPercent gc cpu)
            <> "  " <> p copy
            <> "  " <> pad 14 (pretty (DataRate copy gc :: SomeDataRate))

        p :: forall p. Pretty p => p -> String
        p = pad 12 . pretty

        pad :: Int -> String -> String
        pad n s =
          let l = length s
          in replicate (n - l) ' ' <> s


mkBenchResult :: SomeCount -> GCStats -> GCStats -> BenchResult
mkBenchResult runs@(Count c@(realToFrac -> rs)) before after =
  let rts       = mkRuntimeStats "" before after
      dur       = cputime rts
      cpu       = cputime rts / rs
      mut       = mutTime rts / rs
      gc        = gcTime  rts / rs
      alloc     = allocated rts `div` fromIntegral c
      allocRate = DataRate alloc mut -- allocation per MUT second, like -RTS +s
      copy      = Bytes (fromIntegral (copied rts) `div` c)
      copyRate  = DataRate copy gc
      coll      = collections rts
  in BenchResult {..}

{-# INLINE whnf #-}
whnf :: (NFData a) => String -> (b -> a) -> b -> Test Sync BenchResult
whnf nm f b = scope nm $ do
    note "Preparing benchmark..."
    !br <- io $ do
      yield
      execute 1
    runBenchmarks br
  where
    {-# INLINE runBenchmarks #-}
    runBenchmarks :: BenchResult -> Test Sync BenchResult
    runBenchmarks br
      | dur br > (2 * 10^12) = do
          note "Setup took longer than 2 seconds; not performing iteration."
          note "Consider simplifying your benchmark for more meaningful results."
          return br

      | dur br < Milliseconds 10 = do
          br <- io $ execute (10 * runs br)
          runBenchmarks br

      | otherwise = do
          let rs :: Int64
              rs = round $ 5 / realToFrac (dur br) * realToFrac (runs br)
          note $ "Running..."
          io $ do
            yield
            execute (Count rs)

    {-# INLINE execute #-}
    execute :: SomeCount -> IO BenchResult
    execute (Count n) = do
        performGC
        !before <- getGCStats
        go n
        performGC
        !after <- getGCStats
        return $ mkBenchResult (Count n) before after
      where
        go 0 = return ()
        go n = do
          r <- evaluate $ force $ f b
          go (n - 1)
