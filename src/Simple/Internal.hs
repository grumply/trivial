{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Simple.Internal where

import Simple.Internal.Base
import Simple.Internal.Count
import Simple.Internal.DataRate
import Simple.Internal.Improving
import Simple.Internal.Magnitude
import Simple.Internal.Pretty
import Simple.Internal.Rate
import Simple.Internal.Similar
import Simple.Internal.Space
import Simple.Internal.Time
import Simple.Internal.Percent

import GHC.Generics
import GHC.Stats

import Data.Aeson
import Data.Monoid

{-# INLINE mkBenchResult #-}
mkBenchResult :: String -> GCStats -> GCStats -> BenchResult
mkBenchResult label before after =
    let cpuElapsed  = Seconds (wallSeconds after)        - Seconds (wallSeconds before)
        cputime     = Seconds (cpuSeconds after)         - Seconds (cpuSeconds before)
        mutElapsed  = Seconds (mutatorWallSeconds after) - Seconds (mutatorWallSeconds before)
        mutTime     = Seconds (mutatorCpuSeconds after)  - Seconds (mutatorCpuSeconds before)
        alloc       = Bytes (bytesAllocated after)       - Bytes (bytesAllocated before)
        peak        = Megabytes (peakMegabytesAllocated after) - Megabytes (peakMegabytesAllocated before)
        used        = Bytes (currentBytesUsed after)     - Bytes (currentBytesUsed before)
        cumulative  = Bytes (cumulativeBytesUsed after)  - Bytes (cumulativeBytesUsed before)
        maxBytes    = Bytes (maxBytesUsed after)         - Bytes (maxBytesUsed before)
        gcElapsed   = Seconds (gcWallSeconds after)      - Seconds (gcWallSeconds before)
        gcTime      = Seconds (gcCpuSeconds after)       - Seconds (gcCpuSeconds before)
        copyRate    = Throughput copied cpuElapsed
        collections = Count (numGcs after)               - Count (numGcs before)
        uncollected = Bytes (currentBytesUsed after)     - Bytes (currentBytesUsed before)
        copied      = Bytes (bytesCopied after)          - Bytes (bytesCopied before)
        slop        = Bytes (currentBytesSlop after)     - Bytes (currentBytesSlop before)
    in BenchResult {..}

data BenchResult = BenchResult
    { label        :: !String
    , cpuElapsed   :: {-# UNPACK #-}!Elapsed
    , cputime      :: {-# UNPACK #-}!CPUTime
    , mutElapsed   :: {-# UNPACK #-}!Elapsed
    , mutTime      :: {-# UNPACK #-}!CPUTime
    , alloc        :: {-# UNPACK #-}!Allocated
    -- , mutated      :: {-# UNPACK #-}!Mutated
    , peak         :: {-# UNPACK #-}!Peak
    , used         :: {-# UNPACK #-}!Used
    , cumulative   :: {-# UNPACK #-}!Cumulative
    , maxBytes     :: {-# UNPACK #-}!Max
    , gcElapsed    :: {-# UNPACK #-}!Elapsed
    , gcTime       :: {-# UNPACK #-}!CPUTime
    , copyRate     :: {-# UNPACK #-}!CopyRate
    , collections  :: {-# UNPACK #-}!Collections
    , uncollected  :: {-# UNPACK #-}!Live
    , copied       :: {-# UNPACK #-}!Copied
    , slop         :: {-# UNPACK #-}!Slop
    } deriving (Generic, Read, Show, Eq, ToJSON, FromJSON)

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
            , "Collections:" <> pad 7 (pretty collections)
            , "Leftover:   " <> pad 7 (pretty uncollected)
            ]
      where
        header = "            Time |"
                  <> "    Relative |"
                  <> "       Bytes |"
                  <> "    Throughput"

        divider = "     -------------------------------------------------------"

        cpuTimeStats =
          "CPU:"    <> p cputime

        mutTimeStats =
          "MUT:"    <> p mutTime
            <> "  " <> p (mkPercent mutTime cputime)
            <> "  " <> p alloc
            <> "  " <> pad 14 (pretty (Throughput alloc mutTime :: DataRate))

        gcTimeStats =
          "GC: "    <> p gcTime
            <> "  " <> p (mkPercent gcTime cputime)
            <> "  " <> p copied
            <> "  " <> pad 14 (pretty (Throughput copied gcTime :: DataRate))

        p :: forall p. Pretty p => p -> String
        p = pad 12 . pretty

        pad :: Int -> String -> String
        pad n s =
          let l = length s
          in replicate (n - l) ' ' <> s


    -- let cpuElapsed = wallSeconds after - wallSeconds before
    --     cpuTime    = cpuSeconds after - cpuSeconds before
    --     elapsed = Duration cpuElapsed
    --     time    = Duration cpuTime
    --     factor  = Factor (cpuTime / cpuElapsed)
    --     e = gcWallSeconds after - gcWallSeconds before
    --     t = gcCpuSeconds after - gcCpuSeconds before
    --     elapsed = Duration e
    --     time    = Duration t
    --     factor  = Factor (t / e)
    --     effect  = Percent (e / cpuElapsed)
    --     burden  = Percent (t / cpuTime)
    --     bytes   = Bytes $ bytesCopied after - bytesCopied before
    --     rate    = Throughput $ realToFrac bytes / e
    --     work    = Throughput $ realToFrac bytes / t
    --     colls   = Collections $ numGcs after - numGcs before - 1
    --     live    = Bytes $ currentBytesUsed after - currentBytesUsed before - _GCStats_size
    --     e = mutatorWallSeconds after - mutatorWallSeconds before
    --     t = mutatorCpuSeconds after - mutatorCpuSeconds before
    --     elapsed = Duration e
    --     time    = Duration t
    --     factor  = Factor (t / e)
    --     effect  = Percent (e / cpuElapsed)
    --     burden  = Percent (t / cpuTime)
    --     bytes   = Bytes $ bytesAllocated after - bytesAllocated before - _GCStats_size
    --     rate    = Throughput $ realToFrac bytes / e
    --     work    = Throughput $ realToFrac bytes / t


