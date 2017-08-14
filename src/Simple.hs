{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Simple (module Simple, module Export) where

import Easy as Export

import Simple.Internal
import Simple.Types
import Simple.Internal.Count

import Data.Int
import Data.List (intercalate,foldl')

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Data.Monoid
import GHC.Generics
import GHC.Stats
import GHC.Conc
import System.Mem
import System.CPUTime

import Debug.Trace

import Data.Aeson

import System.IO.Unsafe

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
  , br_dur :: {-# UNPACK #-}!CPUTime
  , br_cpu :: {-# UNPACK #-}!CPUTime
  , br_mut :: {-# UNPACK #-}!CPUTime
  , br_gc  :: {-# UNPACK #-}!CPUTime
  , br_alloc :: {-# UNPACK #-}!Allocated
  , br_copy :: {-# UNPACK #-}!Copied
  , br_coll :: {-# UNPACK #-}!Collections
  } deriving (Show,Read,Eq,Generic,ToJSON,FromJSON)

instance Vary BenchResult
instance Similar BenchResult
instance Magnitude BenchResult

instance Monoid BenchResult where
  {-# INLINE mempty #-}
  mempty = BenchResult 0 0 0 0 0 0 0 0
  {-# INLINE mappend #-}
  mappend = addBenchResult

{-# INLINE allocRate #-}
allocRate :: BenchResult -> AllocationRate
allocRate br = DataRate (alloc br) (mut br) -- allocation per MUT second, like -RTS +s

{-# INLINE copyRate #-}
copyRate :: BenchResult -> CopyRate
copyRate br = DataRate (copy br) (gc br)

{-# INLINE runs #-}
runs :: BenchResult -> SomeCount
runs = br_runs

{-# INLINE dur #-}
dur :: BenchResult -> CPUTime
dur = br_dur

{-# INLINE cpu #-}
cpu :: BenchResult -> CPUTime
cpu BenchResult {..} = br_cpu / realToFrac br_runs

{-# INLINE mut #-}
mut :: BenchResult -> CPUTime
mut BenchResult {..} = br_mut / realToFrac br_runs

{-# INLINE gc #-}
gc :: BenchResult -> CPUTime
gc BenchResult {..} = br_gc / realToFrac br_runs

{-# INLINE alloc #-}
alloc :: BenchResult -> Allocated
alloc BenchResult {..} = br_alloc / realToFrac br_runs

{-# INLINE copy #-}
copy :: BenchResult -> Copied
copy BenchResult {..} = br_copy / realToFrac br_runs

{-# INLINE coll #-}
coll :: BenchResult -> Collections
coll BenchResult {..} = br_coll

{-# INLINE addBenchResult #-}
addBenchResult :: BenchResult -> BenchResult -> BenchResult
addBenchResult br1 br2 =
  let !brruns  = br_runs br1  + br_runs br2
      !brdur   = br_dur br1   + br_dur br2
      !brcpu   = br_cpu br1   + br_cpu br2
      !brmut   = br_mut br1   + br_mut br2
      !brgc    = br_gc br1    + br_gc br2
      !bralloc = br_alloc br1 + br_alloc br2
      !brcopy  = br_copy br1  + br_copy br2
      !brcoll  = br_coll br1  + br_coll br2
  in BenchResult brruns brdur brcpu brmut brgc bralloc brcopy brcoll

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
            , "Duration:   " <> pad 11 (pretty (dur br))
            , "Collections:" <> pad 11 (pretty (coll br))
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
            <> "  " <> pad 14 (pretty (allocRate br))

        gcTimeStats =
          "GC: "    <> p (gc br)
            <> "  " <> p (mkPercent (gc br) (cpu br))
            <> "  " <> p (copy br)
            <> "  " <> pad 14 (pretty (copyRate br))

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
  , bd_alloc  :: {-# UNPACK #-}!SomeFactor
  , bd_copy   :: {-# UNPACK #-}!SomeFactor
  , bd_coll   :: {-# UNPACK #-}!SomeFactor
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
    bd_alloc  = realToFrac (realToFrac (br_alloc br2) / realToFrac (br_runs br2)) / realToFrac (realToFrac (br_alloc br1) / realToFrac (br_runs br1))
    bd_copy   = realToFrac (realToFrac (br_copy br2) / realToFrac (br_runs br2)) / realToFrac (realToFrac (br_copy br1) / realToFrac (br_runs br1))
    bd_coll   = (realToFrac (br_coll br2)) / (realToFrac (br_coll br1))

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
            ]
      where
        prettyFactorReverse :: SomeFactor -> String
        prettyFactorReverse f =
          pad 21 $
            if isNaN f then
              "\27[00m" <> pretty (SomeFactor 0) <> "\27[0m"
            else if f < 0.84 then
              "\27[91m" <> pretty f <> "\27[0m"
            else if f > 1.15 then
              "\27[92m" <> pretty f <> "\27[0m"
            else
              "\27[00m" <> pretty f <> "\27[0m"

        prettyFactor :: SomeFactor -> String
        prettyFactor f =
          pad 21 $
            if isNaN f then
              "\27[00m" <> pretty (SomeFactor 0) <> "\27[0m"
            else if f < 0.85 then
              "\27[92m" <> pretty f <> "\27[0m"
            else if f > 1.15 then
              "\27[91m" <> pretty f <> "\27[0m"
            else
              "\27[00m" <> pretty f <> "\27[0m"

        header2 = "            Time |    Relative |       Space |    Throughput"

        divider = "     -------------------------------------------------------"

        cpuTimeStats =
          "CPU:"    <> prettyFactor bd_cpu

        relativeMutationTime =
          (/) (realToFrac (mkPercent (mut bd_bench1) (cpu bd_bench1)))
              (realToFrac (mkPercent (mut bd_bench2) (cpu bd_bench2)))

        relativeGCTime =
          (/) (realToFrac (mkPercent (gc bd_bench1) (cpu bd_bench1)))
              (realToFrac (mkPercent (gc bd_bench2) (cpu bd_bench2)))

        allocRateFactor =
          (/) (realToFrac (allocRate bd_bench1))
              (realToFrac (allocRate bd_bench2))

        copyRateFactor =
          (/) (realToFrac (copyRate bd_bench1))
              (realToFrac (copyRate bd_bench2))

        mutTimeStats =
          "MUT:"    <> prettyFactor bd_mut
            <> "  " <> prettyFactor relativeMutationTime
            <> "  " <> prettyFactor bd_alloc
            <> "    " <> prettyFactorReverse allocRateFactor

        gcTimeStats =
          "GC: "    <> prettyFactor bd_gc
            <> "  " <> prettyFactor relativeGCTime
            <> "  " <> prettyFactor bd_copy
            <> "    " <> prettyFactorReverse copyRateFactor

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
            , ""
            , "Old Collections:" <> pad 11 (pretty (coll bd_bench1))
            , "New Collections:" <> pad 11 (pretty (coll bd_bench2))
            , "Change:         " <> pad 11 (pretty (Factor (coll bd_bench1) (coll bd_bench2) :: SomeFactor))
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
            <> "  " <> pad 14 (pretty (allocRate bd_bench1))

        newMutTimeStats =
          "New MUT:"    <> p (mut bd_bench2)
            <> "  " <> p (mkPercent (mut bd_bench2) (cpu bd_bench2))
            <> "  " <> p (alloc bd_bench2)
            <> "  " <> pad 14 (pretty (allocRate bd_bench2))

        oldGcTimeStats =
          "Old GC: "    <> p (gc bd_bench1)
            <> "  " <> p (mkPercent (gc bd_bench1) (cpu bd_bench1))
            <> "  " <> p (copy bd_bench1)
            <> "  " <> pad 14 (pretty (copyRate bd_bench1))

        newGcTimeStats =
          "New GC: "    <> p (gc bd_bench2)
            <> "  " <> p (mkPercent (gc bd_bench2) (cpu bd_bench2))
            <> "  " <> p (copy bd_bench2)
            <> "  " <> pad 14 (pretty (copyRate bd_bench2))

        prettyFactorReverse :: SomeFactor -> String
        prettyFactorReverse f =
          pad 21 $
            if isNaN f then
              "\27[00m" <> pretty (SomeFactor 0) <> "\27[0m"
            else if f < 0.84 then
              "\27[91m" <> pretty f <> "\27[0m"
            else if f > 1.15 then
              "\27[92m" <> pretty f <> "\27[0m"
            else
              "\27[00m" <> pretty f <> "\27[0m"

        prettyFactor :: SomeFactor -> String
        prettyFactor f =
          pad 21 $
            if isNaN f then
              "\27[00m" <> pretty (SomeFactor 0) <> "\27[0m"
            else if f < 0.85 then
              "\27[92m" <> pretty f <> "\27[0m"
            else if f > 1.15 then
              "\27[91m" <> pretty f <> "\27[0m"
            else
              "\27[00m" <> pretty f <> "\27[0m"

        header2 = "                Time |    Relative |       Space |    Throughput"

        divider = "         -------------------------------------------------------"

        cpuTimeDiff =
          "Change: "    <> prettyFactor bd_cpu

        relativeMutationTime =
          (/) (realToFrac (mkPercent (mut bd_bench1) (cpu bd_bench1)))
              (realToFrac (mkPercent (mut bd_bench2) (cpu bd_bench2)))

        relativeGCTime =
          (/) (realToFrac (mkPercent (gc bd_bench1) (cpu bd_bench1)))
              (realToFrac (mkPercent (gc bd_bench2) (cpu bd_bench2)))

        allocRateFactor =
          (/) (realToFrac (allocRate bd_bench1))
              (realToFrac (allocRate bd_bench2))

        copyRateFactor =
          (/) (realToFrac (copyRate bd_bench1))
              (realToFrac (copyRate bd_bench2))

        mutTimeDiff =
          "Change: "    <> prettyFactor bd_mut
            <> "  " <> prettyFactor relativeMutationTime
            <> "  " <> prettyFactor bd_alloc
            <> "    " <> prettyFactor allocRateFactor

        gcTimeDiff =
          "Change: "    <> prettyFactor bd_gc
            <> "  " <> prettyFactor relativeGCTime
            <> "  " <> prettyFactor bd_copy
            <> "    " <> prettyFactor copyRateFactor

        p :: forall p. Pretty p => p -> String
        p = pad 12 . pretty

        pad :: Int -> String -> String
        pad n s =
          let l = length s
          in replicate (n - l) ' ' <> s



{-# INLINE mkBenchResult #-}
mkBenchResult :: SomeCount -> GCStats -> GCStats -> BenchResult
mkBenchResult br_runs before after =
  let !rts      = mkRuntimeStats "" before after
      !br_dur   = rs_cputime rts
      !br_cpu   = rs_cputime rts
      !br_mut   = rs_mutTime rts
      !br_gc    = rs_gcTime  rts
      !br_alloc = rs_allocated rts
      !br_copy  = rs_copied rts
      !br_coll  = rs_collections rts
  in BenchResult {..}

removeOverhead :: BenchResult -> BenchResult -> BenchResult
removeOverhead overhead br =
  BenchResult
    (br_runs br)
    (max 0 (br_dur br - br_dur overhead))
    (max 0 (br_cpu br - br_cpu overhead))
    (max 0 (br_mut br - br_mut overhead))
    (br_gc br)
    (br_alloc br)
    (br_copy br)
    (br_coll br)

{-# INLINE nf #-}
nf :: (NFData a) => String -> (b -> a) -> b -> Test Sync BenchResult
nf nm f b = scope nm $ do
    noteScoped "running..."
    io $ do
      yield
      let rs = 1
      (before,after) <- execute (round rs)
      run (mkBenchResult rs before after)
  where
    {-# INLINE run #-}
    run :: BenchResult -> IO BenchResult
    run br
      | dur br < Milliseconds 10 = do
          let rs = 10 * runs br
          (before,after) <- execute (round rs)
          run $ mkBenchResult rs before after

      | otherwise = do
          let rs = realToFrac $ (Seconds 5 / dur br) * realToFrac (runs br)
          (before,after) <- execute (round rs)
          return $ mkBenchResult rs before after

    {-# INLINE execute #-}
    execute :: Int64 -> IO (GCStats,GCStats)
    execute n = do
        performGC
        before <- getGCStats
        go n f b
        performGC
        after <- getGCStats
        return (before,after)

      where
        {-# INLINE go #-}
        go 0 f b = return ()
        go n f b = f b `deepseq` go (n - 1) f b

{-# INLINE nfio #-}
nfio :: (NFData a) => String -> IO a -> Test Sync BenchResult
nfio nm f = scope nm $ do
    noteScoped "running..."
    io $ go mempty
  where
    {-# INLINE go #-}
    go :: BenchResult -> IO BenchResult
    go br = do
      (before,after) <- execute
      let !br' = mkBenchResult (Count 1) before after
          !br'' = br <> br'
      if dur br'' > Seconds 5 then
        return br''
      else
        go br''

    {-# INLINE execute #-}
    execute :: IO (GCStats,GCStats)
    execute = do
        performGC
        before <- getGCStats
        a <- f
        a `deepseq` performGC
        after <- getGCStats
        return (before,after)

{-# INLINE nfwithCleanup #-}
nfwithCleanup :: (NFData env, NFData a) => String -> (Int64 -> IO env) -> (Int64 -> env -> IO a) -> (env -> IO b) -> Test Sync BenchResult
nfwithCleanup nm alloc act cleanup = do
    noteScoped "running..."
    io $ go 1 mempty
  where
    {-# INLINE go #-}
    go :: Int64 -> BenchResult -> IO BenchResult
    go !c br = do
      (before,after) <- execute c
      let !br' = mkBenchResult (Count 1) before after
          !br'' = br <> br'
      if dur br'' > Seconds 5 then
        return br''
      else
        go (c + 1) br''

    {-# INLINE execute #-}
    execute :: Int64 -> IO (GCStats,GCStats)
    execute n = do
        env <- alloc n
        env <- evaluate $ force env
        performGC
        before <- getGCStats
        a <- act n env
        a `deepseq` performGC
        after <- getGCStats
        cleanup env
        return (before,after)

{-# INLINE nfwith #-}
nfwith :: (NFData env, NFData a) => String -> (Int64 -> IO env) -> (Int64 -> env -> IO a) -> Test Sync BenchResult
nfwith nm alloc act = do
    noteScoped "running..."
    io $ go 1 mempty
  where
    {-# INLINE go #-}
    go :: Int64 -> BenchResult -> IO BenchResult
    go !c br = do
      (before,after) <- execute c
      let !br' = mkBenchResult (Count 1) before after
          !br'' = br <> br'
      if dur br'' > Seconds 5 then
        return br''
      else
        go (c + 1) br''

    {-# INLINE execute #-}
    execute :: Int64 -> IO (GCStats,GCStats)
    execute n = do
        env <- alloc n
        env <- evaluate $ force env
        performGC
        before <- getGCStats
        a <- act n env
        a `deepseq` performGC
        after <- getGCStats
        return (before,after)

{-# INLINE whnf #-}
whnf :: String -> (b -> a) -> b -> Test Sync BenchResult
whnf nm f b = scope nm $ do
    noteScoped "running..."
    io $ do
      yield
      let rs = 1
      (before,after) <- execute (round rs)
      run (mkBenchResult rs before after)
  where
    {-# INLINE run #-}
    run :: BenchResult -> IO BenchResult
    run br
      | dur br < Milliseconds 10 = do
          let rs = 10 * runs br
          (before,after) <- execute (round rs)
          run $ mkBenchResult rs before after

      | otherwise = do
          let rs = realToFrac $ (Seconds 5 / dur br) * realToFrac (runs br)
          (before,after) <- execute (round rs)
          return $ mkBenchResult rs before after

    {-# INLINE execute #-}
    execute :: Int64 -> IO (GCStats,GCStats)
    execute n = do
        performGC
        before <- getGCStats
        go n f b
        performGC
        after <- getGCStats
        return (before,after)

      where
        {-# INLINE go #-}
        go 0 f b = return ()
        go n f b = f b `seq` go (n - 1) f b

{-# INLINE whnfwith #-}
whnfwith :: (NFData env) => String -> (Int64 -> IO env) -> (Int64 -> env -> IO a) -> Test Sync BenchResult
whnfwith nm alloc act = do
    noteScoped "running..."
    io $ go 1 mempty
  where
    {-# INLINE go #-}
    go :: Int64 -> BenchResult -> IO BenchResult
    go c br = do
      (before,after) <- execute c
      let !br' = mkBenchResult (Count 1) before after
          !br'' = br <> br'
      if dur br'' > Seconds 5 then
        return br''
      else
        go (c + 1) br''

    {-# INLINE execute #-}
    execute :: Int64 -> IO (GCStats,GCStats)
    execute n = do
        env <- alloc n
        env <- evaluate $ force env
        performGC
        before <- getGCStats
        a <- act n env
        a `seq` performGC
        after <- getGCStats
        return (before,after)

{-# INLINE whnfio #-}
whnfio :: String -> IO a -> Test Sync BenchResult
whnfio nm act = scope nm $ do
    noteScoped "running..."
    io $ go mempty
  where
    {-# INLINE go #-}
    go :: BenchResult -> IO BenchResult
    go br = do
      (before,after) <- execute
      let !br' = mkBenchResult (Count 1) before after
          !br'' = br <> br'
      if dur br'' > Seconds 5 then
        return br''
      else
        go br''

    {-# INLINE execute #-}
    execute :: IO (GCStats,GCStats)
    execute = do
        performGC
        before <- getGCStats
        a <- act
        a `seq` performGC
        after <- getGCStats
        return (before,after)

{-# INLINE whnfwithCleanup #-}
whnfwithCleanup :: (NFData env) => String -> (Int64 -> IO env) -> (Int64 -> env -> IO a) -> (env -> IO b) -> Test Sync BenchResult
whnfwithCleanup nm alloc act cleanup = do
    noteScoped "running..."
    io $ go 1 mempty
  where
    {-# INLINE go #-}
    go :: Int64 -> BenchResult -> IO BenchResult
    go c br = do
      (before,after) <- execute c
      let !br' = mkBenchResult (Count 1) before after
          !br'' = br <> br'
      if dur br'' > Seconds 5 then
        return br''
      else
        go (c + 1) br''

    {-# INLINE execute #-}
    execute :: Int64 -> IO (GCStats,GCStats)
    execute n = do
        env <- alloc n
        env <- evaluate $ force env
        performGC
        before <- getGCStats
        a <- act n env
        a `seq` performGC
        after <- getGCStats
        cleanup env
        return (before,after)

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

            Garbage -> M copy
            Copy    -> M copyRate
            GCs     -> M coll

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
