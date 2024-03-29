{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Simple.GHCJS (module Simple.GHCJS, module Export) where

import Easy as Export

import Simple.Types
import Simple.Internal.Count

import Data.Int
import Data.List (intercalate,foldl')

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Data.Monoid
import GHC.Generics
import GHC.Conc
import qualified System.Mem as GHC

import Data.Aeson

import GHCJS.Types

import System.IO.Unsafe

import Debug.Trace

chrome_needed f = do
  unless can_gc $ io $ do
    install_facade_gc
    putStrLn chrome_needed_message
  f
  where
    chrome_needed_message =
      concat [ "Benchmarking will only produce useful GC results in Google Chrome started with "
             , "`--args --enable-precise-memory-info --enable-memory-info --js-flags=\"--expose-gc\"`.\n"
             , "Without Chrome's memory statistics, this library will only produce timing results."
             ]

foreign import javascript unsafe
  "$r = window.performance.memory && 96 || 0" memory_stats_overhead :: Int

foreign import javascript unsafe
  "$r = typeof window.gc === 'function'" can_gc :: Bool

foreign import javascript unsafe
  "window.gc = function() { return; };" install_facade_gc :: IO ()

foreign import javascript unsafe
  "$r = window.performance.now() / 1000;" get_cpu_time_js :: IO Elapsed

foreign import javascript unsafe
  "window.gc();" perform_gc_js :: IO ()

foreign import javascript unsafe
  "$r = window.performance.memory || {}" get_memory_stats_js :: IO JSVal

foreign import javascript unsafe
  "$r = $1.totalJSHeapSize || 0" total_js_heap_size_js :: JSVal -> Int

foreign import javascript unsafe
  "$r = $1.usedJSHeapSize || 0" used_js_heap_size_js :: JSVal -> Int

foreign import javascript unsafe
  "$r = $1.jsHeapSizeLimit || 0" js_heap_size_limit_js :: JSVal -> Int

type Benchmark = Test Sync ()

withEnv :: (NFData env) => String -> Test Sync env -> (env -> Benchmark) -> Benchmark
withEnv nm mkenv f = do
  e <- mkenv
  io $ evaluate $ rnf e
  f e

withEnvCleanup :: (NFData env) => String -> Test Sync env -> (env -> Benchmark) -> (env -> Benchmark) -> Benchmark
withEnvCleanup nm mkenv f c = withEnv nm mkenv (\env -> f env >> c env)

data BenchResult = BenchResult
  { br_runs :: {-# UNPACK #-}!SomeCount
  , br_dur :: {-# UNPACK #-}!Elapsed
  , br_cpu :: {-# UNPACK #-}!Elapsed
  , br_mut :: {-# UNPACK #-}!Elapsed
  , br_gc  :: {-# UNPACK #-}!Elapsed
  , br_js_gc :: {-# UNPACK #-}!Elapsed
  , br_alloc :: {-# UNPACK #-}!Allocated
  , br_dealloc :: {-# UNPACK #-}!Deallocated
  } deriving (Show,Read,Eq,Generic,ToJSON,FromJSON)

instance Vary BenchResult
instance Similar BenchResult
instance Magnitude BenchResult

instance Monoid BenchResult where
  {-# INLINE mempty #-}
  mempty = BenchResult 0 0 0 0 0 0 0 0
  {-# INLINE mappend #-}
  mappend br1 br2 =
    let !brruns  = br_runs br1  + br_runs br2
        !brdur   = br_dur br1   + br_dur br2
        !brcpu   = br_cpu br1   + br_cpu br2
        !brmut   = br_mut br1   + br_mut br2
        !brgc    = br_gc br1    + br_gc br2
        !brjsgc  = br_js_gc br1 + br_js_gc br2
        !bralloc = br_alloc br1 + br_alloc br2
        !brdealloc = br_dealloc br1 + br_dealloc br2
    in BenchResult brruns brdur brcpu brmut brgc brjsgc bralloc brdealloc

{-# INLINE allocRate #-}
allocRate :: BenchResult -> AllocationRate
allocRate br = DataRate (alloc br) (mut br) -- allocation per MUT second, like -RTS +s

{-# INLINE deallocRate #-}
deallocRate :: BenchResult -> DeallocationRate
deallocRate br = DataRate (dealloc br) (gc br) -- deallocation per GC second

{-# INLINE runs #-}
runs :: BenchResult -> SomeCount
runs = br_runs

{-# INLINE dur #-}
dur :: BenchResult -> Elapsed
dur = br_dur

{-# INLINE cpu #-}
cpu :: BenchResult -> Elapsed
cpu BenchResult {..} = br_cpu / realToFrac br_runs

{-# INLINE mut #-}
mut :: BenchResult -> Elapsed
mut BenchResult {..} = br_mut / realToFrac br_runs

{-# INLINE gc #-}
gc :: BenchResult -> Elapsed
gc BenchResult {..} = br_gc / realToFrac br_runs

{-# INLINE alloc #-}
alloc :: BenchResult -> Allocated
alloc BenchResult {..} = br_alloc / realToFrac br_runs

{-# INLINE dealloc #-}
dealloc :: BenchResult -> Deallocated
dealloc BenchResult {..} = br_dealloc / realToFrac br_runs

instance Pretty BenchResult where
    pretty br@BenchResult {..} =
        unlines
            [ ""
            , divider
            , header2
            , divider
            , cpuTimeStats
            , mutTimeStats
            , gcTimeStats
            , ""
            , "Runs:       " <> pad 11 (pretty (runs br))
            , "Bytes:      " <> pad 11 (pretty br_alloc)
            ]
      where
        header2 = "                Time           |       Space |    Throughput"
        divider = "     -------------------------------------------------------"

        cpuTimeStats =
          "CPU:"    <> p (cpu br)

        mutTimeStats =
          "MUT:"    <> p (mut br)
            <> "  " <> p (mkPercent (mut br) (cpu br))
            <> "  " <> p (alloc br)
            <> "  " <> p (allocRate br)

        gcTimeStats =
          "GC: "    <> p (gc br)
            <> "  " <> p (mkPercent (gc br) (cpu br))
            <> "  " <> p (dealloc br)
            <> "  " <> p (deallocRate br)

        p :: forall p. Pretty p => p -> String
        p = pad 12 . pretty

        pad :: Int -> String -> String
        pad n s =
          let l = length s
          in replicate (n - l) ' ' <> s

data BenchDiff = BenchDiff
  { bd_bench1 :: {-# UNPACK #-}!BenchResult
  , bd_bench2 :: {-# UNPACK #-}!BenchResult
  , bd_cpu    :: {-# UNPACK #-}!SomeFactor
  , bd_mut    :: {-# UNPACK #-}!SomeFactor
  , bd_gc     :: {-# UNPACK #-}!SomeFactor
  , bd_js_gc  :: {-# UNPACK #-}!SomeFactor
  , bd_alloc  :: {-# UNPACK #-}!SomeFactor
  , bd_dealloc :: {-# UNPACK #-}!SomeFactor
  } deriving (Read,Show,Eq,Generic,ToJSON,FromJSON)

{-# INLINE diff #-}
diff :: BenchResult -> BenchResult -> BenchDiff
diff br1 br2 = BenchDiff {..}
  where
    -- little over-judicious with the realToFrac calls here; try to reduce
    bd_bench1 = br1
    bd_bench2 = br2
    bd_cpu    = realToFrac (br_cpu br2 / realToFrac (br_runs br2)) / realToFrac (br_cpu br1 / realToFrac (br_runs br1))
    bd_mut    = realToFrac (br_mut br2 / realToFrac (br_runs br2)) / realToFrac (br_mut br1 / realToFrac (br_runs br1))
    bd_gc     = realToFrac (br_gc br2 / realToFrac (br_runs br2)) / realToFrac (br_gc br1 / realToFrac (br_runs br1))
    bd_js_gc  = realToFrac (br_js_gc br2 / realToFrac (br_runs br2)) / realToFrac (br_js_gc br1 / realToFrac (br_runs br1))
    bd_alloc  = realToFrac (realToFrac (br_alloc br2) / realToFrac (br_runs br2)) / realToFrac (realToFrac (br_alloc br1) / realToFrac (br_runs br1))
    bd_dealloc = realToFrac (realToFrac (br_dealloc br2) / realToFrac (br_runs br2)) / realToFrac (realToFrac (br_dealloc br1) / realToFrac (br_runs br1))

instance Pretty BenchDiff where
    pretty BenchDiff {..} =
        unlines
            [ ""
            , divider
            , header2
            , divider
            , cpuTimeStats
            , mutTimeStats
            , gcTimeStats
            -- This information wasn't useful or accurate.
            -- , ""
            -- , "Old Bytes:      " <> pad 11 (pretty (br_alloc bd_bench1)) <> " in " <> pretty (br_runs bd_bench1) <> " runs"
            -- , "New Bytes:      " <> pad 11 (pretty (br_alloc bd_bench2)) <> " in " <> pretty (br_runs bd_bench2) <> " runs"
            ]
      where
        header2 = "            Time |    Relative |       Space |    Throughput"

        divider = "     -------------------------------------------------------"

        cpuTimeStats =
          "CPU:"    <> p bd_cpu

        relativeMutationTime :: SomeFactor
        relativeMutationTime =
            Factor
                (mkPercent (mut bd_bench2) (cpu bd_bench2))
                (mkPercent (mut bd_bench1) (cpu bd_bench1))

        relativeGCTime :: SomeFactor
        relativeGCTime =
            Factor
                (mkPercent (gc bd_bench2) (cpu bd_bench2))
                (mkPercent (gc bd_bench1) (cpu bd_bench1))

        allocRateFactor :: SomeFactor
        allocRateFactor =
            Factor
                (allocRate bd_bench2)
                (allocRate bd_bench1)

        deallocRateFactor :: SomeFactor
        deallocRateFactor =
            Factor
                (deallocRate bd_bench2)
                (deallocRate bd_bench1)

        mutTimeStats =
          "MUT:"    <> p bd_mut
            <> "  " <> p relativeMutationTime
            <> "  " <> p bd_alloc
            <> "    " <> p allocRateFactor

        gcTimeStats =
          "GC: "    <> p bd_gc
            <> "  " <> p relativeGCTime
            <> "  " <> p bd_dealloc
            <> "    " <> p deallocRateFactor

        p :: forall p. Pretty p => p -> String
        p = pad 12 . pretty

        pad :: Int -> String -> String
        pad n s =
          let l = length s
          in replicate (n - l) ' ' <> s

report :: BenchResult -> BenchResult -> Benchmark
report br1 br2 = notep $ Report (diff br1 br2)

newtype Report = Report BenchDiff
  deriving (Read,Show,Eq,Generic,ToJSON,FromJSON)

instance Pretty Report where
    pretty (Report (BenchDiff {..})) =
        unlines
            [ ""
            , divider
            , header2
            , divider
            , oldCpuTimeStats
            , newCpuTimeStats
            , cpuTimeDiff
            , ""
            , oldMutTimeStats
            , newMutTimeStats
            , mutTimeDiff
            , ""
            , oldGcTimeStats
            , newGcTimeStats
            , gcTimeDiff
            -- This information wasn't useful or accurate.
            -- , ""
            -- , "Old Bytes:      " <> pad 11 (pretty (br_alloc bd_bench1)) <> " in " <> pretty (br_runs bd_bench1) <> " runs"
            -- , "New Bytes:      " <> pad 11 (pretty (br_alloc bd_bench2)) <> " in " <> pretty (br_runs bd_bench2) <> " runs"
            ]
      where
        oldCpuTimeStats =
          "Old CPU:" <> p (cpu bd_bench1)

        newCpuTimeStats =
          "New CPU:" <> p (cpu bd_bench2)

        oldMutTimeStats =
          "Old MUT:"    <> p (mut bd_bench1)
            <> "  " <> p (mkPercent (mut bd_bench1) (cpu bd_bench1))
            <> "  " <> p (alloc bd_bench1)
            <> "  " <> p (allocRate bd_bench1)

        newMutTimeStats =
          "New MUT:"    <> p (mut bd_bench2)
            <> "  " <> p (mkPercent (mut bd_bench2) (cpu bd_bench2))
            <> "  " <> p (alloc bd_bench2)
            <> "  " <> p (allocRate bd_bench2)

        oldGcTimeStats =
          "Old GC: "    <> p (gc bd_bench1)
            <> "  " <> p (mkPercent (gc bd_bench1) (cpu bd_bench1))
            <> "  " <> p (dealloc bd_bench1)
            <> "  " <> p (deallocRate bd_bench1)

        newGcTimeStats =
          "New GC: "    <> p (gc bd_bench2)
            <> "  " <> p (mkPercent (gc bd_bench2) (cpu bd_bench2))
            <> "  " <> p (dealloc bd_bench2)
            <> "  " <> p (deallocRate bd_bench2)

        header2 = "                Time |    Relative |       Space |    Throughput"

        divider = "         -------------------------------------------------------"

        cpuTimeDiff =
          "Change: "    <> p bd_cpu

        relativeMutationTime :: SomeFactor
        relativeMutationTime =
          Factor (mkPercent (mut bd_bench2) (cpu bd_bench2))
                 (mkPercent (mut bd_bench1) (cpu bd_bench1))

        relativeGCTime :: SomeFactor
        relativeGCTime =
          Factor (mkPercent (gc bd_bench2) (cpu bd_bench2))
                 (mkPercent (gc bd_bench1) (cpu bd_bench1))

        allocRateFactor :: SomeFactor
        allocRateFactor =
          Factor (allocRate bd_bench2) (allocRate bd_bench1)

        deallocRateFactor :: SomeFactor
        deallocRateFactor =
          Factor (deallocRate bd_bench2) (deallocRate bd_bench1)

        mutTimeDiff =
          "Change: "  <> p bd_mut
            <> "  "   <> p relativeMutationTime
            <> "  "   <> p bd_alloc
            <> "  " <> p allocRateFactor

        gcTimeDiff =
          "Change: "  <> p bd_gc
            <> "  "   <> p relativeGCTime
            <> "  "   <> p bd_dealloc
            <> "  " <> p deallocRateFactor

        p :: forall p. Pretty p => p -> String
        p = pad 12 . pretty

        pad :: Int -> String -> String
        pad n s =
          let l = length s
          in replicate (n - l) ' ' <> s

mkBenchResult :: Int64 -> JSVal -> JSVal -> JSVal -> Elapsed -> Elapsed -> Elapsed -> Elapsed -> BenchResult
mkBenchResult n start_stats mid_stats end_stats start_time end_mut_time end_gc_time end_time =
  let br_runs     = fromIntegral n
      !br_dur     = end_time - start_time
      !br_cpu     = end_gc_time - start_time
      !br_mut     = end_mut_time - start_time
      !br_gc      = end_gc_time - end_mut_time
      !br_js_gc   = end_time - end_gc_time
      !br_alloc   = realToFrac $ used_js_heap_size_js mid_stats - used_js_heap_size_js start_stats - memory_stats_overhead
      !br_dealloc = realToFrac $ used_js_heap_size_js mid_stats - used_js_heap_size_js end_stats - memory_stats_overhead
  in BenchResult {..}

removeOverhead :: BenchResult -> BenchResult -> BenchResult
removeOverhead overhead br =
  BenchResult
    (br_runs br)
    (br_dur br)
    (max 0 (br_cpu br - br_cpu overhead))
    (max 0 (br_mut br - br_mut overhead))
    (br_gc br)
    (br_js_gc br)
    (br_alloc br)
    (br_dealloc br)

{-# INLINE nf #-}
nf :: (NFData a) => String -> (b -> a) -> b -> Test Sync BenchResult
nf nm f b = chrome_needed $ scope nm $ do
    noteScoped "running..."
    br <- io $ do
      let rs = 1
      br <- execute (round rs)
      run br
    complete
    return br
  where
    {-# INLINE run #-}
    run :: BenchResult -> IO BenchResult
    run br
      | br_dur br < Milliseconds 100 = do
          let rs = 10 * runs br
          br' <- execute (round rs)
          run br'

      | otherwise = do
          let rs = realToFrac $ Seconds 5 / dur br * realToFrac (runs br)
          execute (round rs)

    {-# INLINE execute #-}
    execute :: Int64 -> IO BenchResult
    execute n = do

        -- initial cleanup and initial stats and time
        GHC.performGC
        perform_gc_js
        start_stats <- get_memory_stats_js
        start_time  <- get_cpu_time_js

        -- the actual test we're interested in; n iterations of (f $ b)
        go n f b

        mid_stats <- get_memory_stats_js

        -- execution time that will undoubtedly include some GC time from js
        -- and may include some GC time from ghcrts; control GHC gc with
        -- a long GC interval - this assumes GHC.performGC doesn't bypass
        -- resetting a last_gc_time value somehow
        end_mut_time <- get_cpu_time_js

        GHC.performGC

        -- gc exeuction time that may include some gc time from js
        end_gc_time <- get_cpu_time_js

        perform_gc_js

        -- js gc time that should not include ghcrts gc in the general case
        end_time <- get_cpu_time_js

        -- final memory stats
        end_stats <- get_memory_stats_js

        return $ mkBenchResult n start_stats mid_stats end_stats start_time end_mut_time end_gc_time end_time

      where
        {-# INLINE go #-}
        go 0 f b = return ()
        go n f b = f b `deepseq` go (n - 1) f b

{-# INLINE nfio #-}
nfio :: (NFData a) => String -> IO a -> Test Sync BenchResult
nfio nm f = chrome_needed $ scope nm $ do
    noteScoped "running..."
    br <- io $ go mempty
    complete
    return br
  where
    {-# INLINE go #-}
    go :: BenchResult -> IO BenchResult
    go br = do
      br' <- execute
      let !br'' = br <> br'
      if dur br'' > Seconds 5 then
        return br''
      else
        go br''

    {-# INLINE execute #-}
    execute :: IO BenchResult
    execute = do

        -- initial cleanup and initial stats and time
        GHC.performGC
        perform_gc_js
        start_stats <- get_memory_stats_js
        start_time  <- get_cpu_time_js

        -- the actual test we're interested in
        a <- f

        mid_stats <- a `deepseq` get_memory_stats_js

        -- execution time that will undoubtedly include some GC time from js
        -- and may include some GC time from ghcrts; control GHC gc with
        -- a long GC interval - this assumes GHC.performGC doesn't bypass
        -- resetting a last_gc_time value somehow
        end_mut_time <- get_cpu_time_js

        GHC.performGC

        -- gc exeuction time that may include some gc time from js
        end_gc_time <- get_cpu_time_js

        perform_gc_js

        -- js gc time that should not include ghcrts gc in the general case
        end_time <- get_cpu_time_js

        -- final memory stats
        end_stats <- get_memory_stats_js

        return $ mkBenchResult 1 start_stats mid_stats end_stats start_time end_mut_time end_gc_time end_time


{-# INLINE nfwithCleanup #-}
nfwithCleanup :: (NFData env, NFData a) => String -> (Int64 -> IO env) -> (Int64 -> env -> IO a) -> (env -> IO b) -> Test Sync BenchResult
nfwithCleanup nm alloc act cleanup = chrome_needed $ scope nm $ do
    noteScoped "running..."
    br <- io $ go 1 mempty
    complete
    return br
  where
    {-# INLINE go #-}
    go :: Int64 -> BenchResult -> IO BenchResult
    go !c br = do
      br' <- execute c
      let !br'' = br <> br'
      if dur br'' > Seconds 5 then
        return br''
      else
        go (c + 1) br''

    {-# INLINE execute #-}
    execute :: Int64 -> IO BenchResult
    execute n = do

        -- setup
        env <- alloc n
        env <- evaluate $ force env

        -- initial cleanup and initial stats and time
        GHC.performGC
        perform_gc_js
        start_stats <- get_memory_stats_js
        start_time  <- get_cpu_time_js

        -- the actual test we're interested in
        a <- act n env

        mid_stats <- a `deepseq` get_memory_stats_js

        -- execution time that will undoubtedly include some GC time from js
        -- and may include some GC time from ghcrts; control GHC gc with
        -- a long GC interval - this assumes GHC.performGC doesn't bypass
        -- resetting a last_gc_time value somehow
        end_mut_time <- get_cpu_time_js

        GHC.performGC

        -- gc exeuction time that may include some gc time from js
        end_gc_time <- get_cpu_time_js

        perform_gc_js

        -- js gc time that should not include ghcrts gc in the general case
        end_time <- get_cpu_time_js

        -- final memory stats
        end_stats <- get_memory_stats_js

        cleanup env

        return $ mkBenchResult 1 start_stats mid_stats end_stats start_time end_mut_time end_gc_time end_time

{-# INLINE nfwith #-}
nfwith :: (NFData env, NFData a) => String -> (Int64 -> IO env) -> (Int64 -> env -> IO a) -> Test Sync BenchResult
nfwith nm alloc act = chrome_needed $ scope nm $ do
    noteScoped "running..."
    br <- io $ go 1 mempty
    complete
    return br
  where
    {-# INLINE go #-}
    go :: Int64 -> BenchResult -> IO BenchResult
    go !c br = do
      br' <- execute c
      let !br'' = br <> br'
      if dur br'' > Seconds 5 then
        return br''
      else
        go (c + 1) br''

    {-# INLINE execute #-}
    execute :: Int64 -> IO BenchResult
    execute n = do

        -- setup
        env <- alloc n
        env <- evaluate $ force env

        -- initial cleanup and initial stats and time
        GHC.performGC
        perform_gc_js
        start_stats <- get_memory_stats_js
        start_time  <- get_cpu_time_js

        -- the actual test we're interested in
        a <- act n env

        mid_stats <- a `deepseq` get_memory_stats_js

        -- execution time that will undoubtedly include some GC time from js
        -- and may include some GC time from ghcrts; control GHC gc with
        -- a long GC interval - this assumes GHC.performGC doesn't bypass
        -- resetting a last_gc_time value somehow
        end_mut_time <- get_cpu_time_js

        GHC.performGC

        -- gc exeuction time that may include some gc time from js
        end_gc_time <- get_cpu_time_js

        perform_gc_js

        -- js gc time that should not include ghcrts gc in the general case
        end_time <- get_cpu_time_js

        -- final memory stats
        end_stats <- get_memory_stats_js

        return $ mkBenchResult 1 start_stats mid_stats end_stats start_time end_mut_time end_gc_time end_time

{-# INLINE whnf #-}
whnf :: String -> (b -> a) -> b -> Test Sync BenchResult
whnf nm f b = chrome_needed $ scope nm $ do
    noteScoped "running..."
    br <- io $ do
      let rs = 1
      br <- execute (round rs)
      run br
    complete
    return br
  where
    {-# INLINE run #-}
    run :: BenchResult -> IO BenchResult
    run br
      | dur br < Milliseconds 100 = do
          let rs = 10 * runs br
          br' <- execute (round rs)
          run br'

      | otherwise = do
          let rs = realToFrac $ Seconds 5 / dur br * realToFrac (runs br)
          execute (round rs)

    {-# INLINE execute #-}
    execute :: Int64 -> IO BenchResult
    execute n = do

        -- initial cleanup and initial stats and time
        GHC.performGC
        perform_gc_js
        start_stats <- get_memory_stats_js
        start_time  <- get_cpu_time_js

        -- the actual test we're interested in; n iterations of (f $ b)
        go n f b

        mid_stats <- get_memory_stats_js

        -- execution time that will undoubtedly include some GC time from js
        -- and may include some GC time from ghcrts; control GHC gc with
        -- a long GC interval - this assumes GHC.performGC doesn't bypass
        -- resetting a last_gc_time value somehow
        end_mut_time <- get_cpu_time_js

        GHC.performGC

        -- gc exeuction time that may include some gc time from js
        end_gc_time <- get_cpu_time_js

        perform_gc_js

        -- js gc time that should not include ghcrts gc in the general case
        end_time <- get_cpu_time_js

        -- final memory stats
        end_stats <- get_memory_stats_js

        return $ mkBenchResult n start_stats mid_stats end_stats start_time end_mut_time end_gc_time end_time

      where
        {-# INLINE go #-}
        go 0 f b = return ()
        go n f b = f b `seq` go (n - 1) f b

{-# INLINE whnfwith #-}
whnfwith :: (NFData env) => String -> (Int64 -> IO env) -> (Int64 -> env -> IO a) -> Test Sync BenchResult
whnfwith nm alloc act = chrome_needed $ scope nm $ do
    noteScoped "running..."
    br <- io $ go 1 mempty
    complete
    return br
  where
    {-# INLINE go #-}
    go :: Int64 -> BenchResult -> IO BenchResult
    go !c br = do
      br' <- execute c
      let !br'' = br <> br'
      if dur br'' > Seconds 5 then
        return br''
      else
        go (c + 1) br''

    {-# INLINE execute #-}
    execute :: Int64 -> IO BenchResult
    execute n = do

        -- setup
        env <- alloc n
        env <- evaluate $ force env

        -- initial cleanup and initial stats and time
        GHC.performGC
        perform_gc_js
        start_stats <- get_memory_stats_js
        start_time  <- get_cpu_time_js

        -- the actual test we're interested in
        a <- act n env

        mid_stats <- a `seq` get_memory_stats_js

        -- execution time that will undoubtedly include some GC time from js
        -- and may include some GC time from ghcrts; control GHC gc with
        -- a long GC interval - this assumes GHC.performGC doesn't bypass
        -- resetting a last_gc_time value somehow
        end_mut_time <- get_cpu_time_js

        GHC.performGC

        -- gc exeuction time that may include some gc time from js
        end_gc_time <- get_cpu_time_js

        perform_gc_js

        -- js gc time that should not include ghcrts gc in the general case
        end_time <- get_cpu_time_js

        -- final memory stats
        end_stats <- get_memory_stats_js

        return $ mkBenchResult 1 start_stats mid_stats end_stats start_time end_mut_time end_gc_time end_time

{-# INLINE whnfio #-}
whnfio :: String -> IO a -> Test Sync BenchResult
whnfio nm act = chrome_needed $ scope nm $ do
    noteScoped "running..."
    br <- io $ go mempty
    complete
    return br
  where
    {-# INLINE go #-}
    go :: BenchResult -> IO BenchResult
    go br = do
      br' <- execute
      let !br'' = br <> br'
      if dur br'' > Seconds 5 then
        return br''
      else
        go br''

    {-# INLINE execute #-}
    execute :: IO BenchResult
    execute = do

        -- initial cleanup and initial stats and time
        GHC.performGC
        perform_gc_js
        start_stats <- get_memory_stats_js
        start_time  <- get_cpu_time_js

        -- the actual test we're interested in
        a <- act

        mid_stats <- a `seq` get_memory_stats_js

        -- execution time that will undoubtedly include some GC time from js
        -- and may include some GC time from ghcrts; control GHC gc with
        -- a long GC interval - this assumes GHC.performGC doesn't bypass
        -- resetting a last_gc_time value somehow
        end_mut_time <- get_cpu_time_js

        GHC.performGC

        -- gc exeuction time that may include some gc time from js
        end_gc_time <- get_cpu_time_js

        perform_gc_js

        -- js gc time that should not include ghcrts gc in the general case
        end_time <- get_cpu_time_js

        -- final memory stats
        end_stats <- get_memory_stats_js

        return $ mkBenchResult 1 start_stats mid_stats end_stats start_time end_mut_time end_gc_time end_time

{-# INLINE whnfwithCleanup #-}
whnfwithCleanup :: (NFData env) => String -> (Int64 -> IO env) -> (Int64 -> env -> IO a) -> (env -> IO b) -> Test Sync BenchResult
whnfwithCleanup nm alloc act cleanup = chrome_needed $ scope nm $ do
    noteScoped "running..."
    br <- io $ go 1 mempty
    complete
    return br
  where
    {-# INLINE go #-}
    go :: Int64 -> BenchResult -> IO BenchResult
    go !c br = do
      br' <- execute c
      let !br'' = br <> br'
      if dur br'' > Seconds 5 then
        return br''
      else
        go (c + 1) br''

    {-# INLINE execute #-}
    execute :: Int64 -> IO BenchResult
    execute n = do

        -- setup
        env <- alloc n
        env <- evaluate $ force env

        -- initial cleanup and initial stats and time
        GHC.performGC
        perform_gc_js
        start_stats <- get_memory_stats_js
        start_time  <- get_cpu_time_js

        -- the actual test we're interested in
        a <- act n env

        mid_stats <- a `seq` get_memory_stats_js

        -- execution time that will undoubtedly include some GC time from js
        -- and may include some GC time from ghcrts; control GHC gc with
        -- a long GC interval - this assumes GHC.performGC doesn't bypass
        -- resetting a last_gc_time value somehow
        end_mut_time <- get_cpu_time_js

        GHC.performGC

        -- gc exeuction time that may include some gc time from js
        end_gc_time <- get_cpu_time_js

        perform_gc_js

        -- js gc time that should not include ghcrts gc in the general case
        end_time <- get_cpu_time_js

        -- final memory stats
        end_stats <- get_memory_stats_js

        cleanup env

        return $ mkBenchResult 1 start_stats mid_stats end_stats start_time end_mut_time end_gc_time end_time

type BenchPred a = a -> BenchResult -> BenchResult -> Bool

data Feature = GC | CPU | MUT | Garbage | Copy | GCs | Clock | Allocation | Mutation
  deriving (Eq,Show,Ord,Read,Enum)

data Predicate
  = Feature :>> ()
  | Feature :>  ()
  | Feature :>= ()
  | Feature :=  ()
  | Feature :<= ()
  | Feature :<  ()
  | Feature :<< ()

data Measurable
  = forall a. (Magnitude a, Base a, Improving a, Similar a, Pretty a) => M (BenchResult -> a)

{-# INLINE constrain #-}
constrain :: BenchResult -> BenchResult -> [Predicate] -> Test sync ()
constrain br1 br2 =
  mapM_ $ \p ->
    let pred =
          case p of
            f :>> () ->
              (" :>>",f,\(M s) -> improving (s br1) (s br2) && not (mag (base (s br1)) (s br1) (s br2)))
            f :> () ->
              (" :>",f,\(M s) -> improving (s br1) (s  br2) && not (sim (base (s br1)) (s br1) (s br2)))
            f :>= () ->
              (" :>=",f,\(M s) -> improving (s br1) (s br2) || sim (base (s br1)) (s br1) (s br2))
            f := () ->
              (" :=",f,\(M s) -> sim (base (s br1)) (s br1) (s br2))
            f :<= () ->
              (" :<=",f,\(M s) -> improving (s br2) (s br1) || sim (base (s br1)) (s br1) (s br2))
            f :< () ->
              (" :<",f,\(M s) -> improving (s br2) (s br1) && not (sim (base (s br1)) (s br1) (s br2)))
            f :<< () ->
              (" :<<",f,\(M s) -> improving (s br2) (s br1) && not (mag (base (s br1)) (s br1) (s br2)))
        selector f =
          case f of
            GC  -> M gc
            CPU -> M cpu
            MUT -> M mut

            Garbage -> M dealloc
            Copy    -> M deallocRate

            Allocation -> M alloc
            Mutation   -> M allocRate

    in case pred of
        (sc,f,g) -> scope (show f ++ sc) $
          let sel = selector f in
          if g sel then
            ok
          else
            case sel of
              M s -> crash $
                intercalate " " [ "Expecting:", pretty (s br1), improvingShow (s br1), pretty (s br2) ]
