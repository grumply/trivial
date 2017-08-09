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

class Similar a where
  similar :: Real b => b -> a -> a -> Bool
  default similar :: (Real b, Real a) => b -> a -> a -> Bool
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
  -- similar :: (Real a,Real b) => a -> b -> b -> Bool
  similar _ 0 0 = True
  similar (realToFrac -> b) (realToFrac -> x) (realToFrac -> y) =
      -- As b ↘ 1, epsilon ↘ 0
      let epsilon = (x :: Double) / (exp 1 * logBase b (x + 1))
          (lo,hi) = (x - epsilon,x + epsilon)
      in lo <= y && y <= hi

instance Similar Double
instance Similar Float
instance Similar Int
instance Similar Integer
instance Similar Int64
instance Similar Int32
instance Similar Int16
instance Similar Int8
instance Similar Word64
instance Similar Word32
instance Similar Word16
instance Similar Word8

instance Similar Duration where
  similar b (Micros d) (Micros d') = similar b d d'

{-# INLINE dissimilar #-}
dissimilar :: (Real a, Similar b) => a -> b -> b -> Bool
dissimilar b x y = not (similar b x y)

{-# INLINE e #-}
e :: Floating a => a
e = exp 1

class Improving a where
  improving :: a -> a -> Bool
  improvingShow :: a -> String
  default improving :: (Real a) => a -> a -> Bool
  improving a b = a < b -- monotonically increasing as default
  default improvingShow :: (Real a) => a -> String
  improvingShow _ = "<"

class Pretty a where
  pretty :: a -> String

instance Pretty [Char] where
  pretty = id

class Time a where
  toTime :: Double -> a
  fromTime :: a -> Double

newtype CPUTime = CPUTime { cpuTime :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,Read,Show)
instance Time CPUTime where
  toTime = CPUTime
  fromTime = cpuTime
instance Improving CPUTime where
  improving d1 d2 = d1 < d2 -- we want more CPUTime and less wall time
  improvingShow _ = "<"
instance Pretty CPUTime where
    pretty (CPUTime d)
        | d < 0.001    = printf     "%.0fμs" (d * 1000000)
        | d < 1        = printf     "%.2fms" (d * 1000)
        | d < 60       = printf     "%.2fs"   d
        | d < 60^2     = printf "%.0fm %ds"  (d / 60)   (roundi d `mod` 60)
        | otherwise    = printf "%.0fh %dm"  (d / 60^2) (roundi d `mod` 60^2)
      where
        roundi :: Double -> Int
        roundi = round

newtype Duration = Duration { duration :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,Read,Show)
instance Time Duration where
  toTime = Duration
  fromTime = duration
instance Improving Duration where
  improving d1 d2 = d1 > d2 -- less time is better
  improvingShow _ = ">"
instance Pretty Duration where
    pretty (Duration d)
        | d < 0.001    = printf     "%.0fμs" (d * 1000000)
        | d < 1        = printf     "%.2fms" (d * 1000)
        | d < 60       = printf     "%.2fs"   d
        | d < 60^2     = printf "%.0fm %ds"  (d / 60)   (roundi d `mod` 60)
        | otherwise    = printf "%.0fh %dm"  (d / 60^2) (roundi d `mod` 60^2)
      where
        roundi :: Double -> Int
        roundi = round

pattern Minutes :: Time t => Double -> t
pattern Minutes s <- ((/60) . fromTime -> s) where
  Minutes s = toTime (s * 60000000)

pattern Seconds :: Time t => Double -> t
pattern Seconds s <- (fromTime -> s) where
  Seconds s = toTime (s * 1000000)

pattern Millis :: Time t => Double -> t
pattern Millis s <- ((* 1000) . fromTime -> s) where
  Millis s = toTime (s / 1000)

pattern Micros :: Time t => Double -> t
pattern Micros s <- ((* 1000000) . fromTime -> s) where
  Micros s = toTime (s / 1000000)

newtype Bytes = Bytes { getBytes :: Int64 }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show)
instance Improving Bytes where
  improving b1 b2 = b1 > b2 -- fewer bytes are better
  improvingShow _ = ">"
instance Pretty Bytes where
    pretty (Bytes b)
        | b < 2^10  = printf "%d B" b
        | b < 2^20  = printf "%.1f KB" (realToFrac b / 2^10 :: Double)
        | b < 2^30  = printf "%.1f MB" (realToFrac b / 2^20 :: Double)
        | b < 2^40  = printf "%.1f GB" (realToFrac b / 2^30 :: Double)
        | otherwise = printf "%.1f TB" (realToFrac b / 2^40 :: Double)
instance Similar Bytes

newtype Throughput = Throughput { getThroughput :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,Read,Show)
instance Improving Throughput -- more throughput is better
instance Pretty Throughput where
    pretty (Throughput t)
        | t < 2^10  = printf "%d B/s"  (round t :: Integer)
        | t < 2^20  = printf "%.1f KB/s" (t / 2^10)
        | t < 2^30  = printf "%.1f MB/s" (t / 2^20)
        | otherwise = printf "%.1f GB/s" (t / 2^30)
instance Similar Throughput

newtype Collections = Collections { getCollections :: Int64 }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show)
instance Improving Collections where
  improving c1 c2 = c1 > c2
  improvingShow _ = ">"
instance Similar Collections
instance Pretty Collections where
  pretty (Collections c) = show c

-- Not specific enough to have an Improving instance;
-- An improvement in one percentage value implies a
-- relative worsening of another.
newtype Percent = Percent { getPercent :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,Read,Show)
instance Pretty Percent where
  pretty (Percent p) = printf "%.2f%%" (p * 100)

newtype Factor = Factor { getFactor :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,Read,Show)
instance Improving Factor -- larger factor is better
instance Pretty Factor where
  pretty (Factor f) = printf "%.2fx" f

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
        , colls        :: {-# UNPACK #-}!Collections-- ^ number of GCs
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
   deriving (Generic, Read, Show, Eq)

data BenchResult = BenchResult
    { label      :: !String
    , cpu        :: !Capability
    , mut        :: !Capability
    , gc         :: !Capability
    } deriving (Generic, Read, Show, Eq)

instance Pretty BenchResult where
    pretty BenchResult {..} =
        unlines
            [ ""
            , header
            , divider
            , cpuTimeStats
            , mutTimeStats
            , gcTimeStats
            , ""
            , "Collections:" <> pad 7 (pretty (colls gc))
            , "Leftover:   " <> pad 7 (pretty (live gc))
            ]
      where
        header = "            Time |"
                  <> "    Relative |"
                  <> "       Bytes |"
                  <> "    Throughput"

        divider = "     -------------------------------------------------------"

        cpuTimeStats =
          "CPU:"    <> p ( time    cpu )

        mutTimeStats =
          "MUT:"    <> p ( time    mut )
            <> "  " <> p ( burden  mut )
            <> "  " <> p ( bytes   mut )
            <> "  " <> pad 14 (pretty ( work    mut ))

        gcTimeStats =
          "GC: "    <> p ( time    gc )
            <> "  " <> p ( burden  gc )
            <> "  " <> p ( bytes   gc )
            <> "  " <> pad 14 (pretty ( work    gc ))

        p :: forall p. Pretty p => p -> String
        p = pad 12 . pretty

        pad :: Int -> String -> String
        pad n s =
          let l = length s
          in replicate (n - l) ' ' <> s



prettyVerbose BenchResult {..} =
    unlines
        [ ""
        , header
        , divider
        , cpuTimeStats
        , mutTimeStats
        , gcTimeStats
        , ""
        , "GC:                      MUT:"
        , "  Bytes: " <> pad 12 (pretty (bytes gc)) <> pad 12 "Bytes:" <> pad 12 (pretty ( bytes mut ))
        , "  Rate:  " <> pad 12 (pretty (rate  gc)) <> pad 12 "Rate: " <> pad 12 (pretty ( rate  mut ))
        , "  Work:  " <> pad 12 (pretty (work  gc)) <> pad 12 "Work: " <> pad 12 (pretty ( work  mut ))
        , "  Live:  " <> pad 12 (pretty (live  gc))
        , "  GCs:   " <> pad 12 (pretty (colls gc))
        ]
  where
    header = "         (E)lapsed |"
              <> "        (T)ime |"
              <> "     (E)/Total |"
              <> "     (T)/Total |"
              <> "       Speedup"

    divider = "     -----------------------------------------------------------------------------"

    cpuTimeStats =
      "CPU:"    <> p ( elapsed cpu )
        <> "  " <> p ( time    cpu )
        <> "  " <> pad 14 ""
        <> "  " <> pad 14 ""
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
    p = pad 14 . pretty

    pad :: Int -> String -> String
    pad n s =
      let l = length s
      in replicate (n - l) ' ' <> s

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

data Retries = Retries Int | NoRetries

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
