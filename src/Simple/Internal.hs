{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BangPatterns #-}
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
import Simple.Internal.Variance

import GHC.Generics
import GHC.Stats

import Data.Aeson
import Data.Monoid

{-# INLINE mkRuntimeStats #-}
mkRuntimeStats :: String -> GCStats -> GCStats -> RuntimeStats
mkRuntimeStats label before after =
    let !rs_cpuElapsed  = Seconds (wallSeconds after)        - Seconds (wallSeconds before)
        !rs_cputime     = Seconds (cpuSeconds after)         - Seconds (cpuSeconds before)
        !rs_mutElapsed  = Seconds (mutatorWallSeconds after) - Seconds (mutatorWallSeconds before)
        !rs_mutTime     = Seconds (mutatorCpuSeconds after)  - Seconds (mutatorCpuSeconds before)
        !rs_allocated   = Bytes (realToFrac $ bytesAllocated after)       - Bytes (realToFrac $ bytesAllocated before) - 152
        !rs_peak        = Megabytes (realToFrac $ peakMegabytesAllocated after) - Megabytes (realToFrac $ peakMegabytesAllocated before)
        !rs_used        = Bytes (realToFrac $ currentBytesUsed after)     - Bytes (realToFrac $ currentBytesUsed before) - 152
        !rs_cumulative  = Bytes (realToFrac $ cumulativeBytesUsed after)  - Bytes (realToFrac $ cumulativeBytesUsed before)
        !rs_maxBytes    = Bytes (realToFrac $ maxBytesUsed after)         - Bytes (realToFrac $ maxBytesUsed before)
        !rs_gcElapsed   = Seconds (gcWallSeconds after)      - Seconds (gcWallSeconds before)
        !rs_gcTime      = Seconds (gcCpuSeconds after)       - Seconds (gcCpuSeconds before)
        !rs_collections = Count (realToFrac $ numGcs after)               - Count (realToFrac $ numGcs before)
        !rs_uncollected = Bytes (realToFrac $ currentBytesUsed after)     - Bytes (realToFrac $ currentBytesUsed before)
        !rs_copied      = Bytes (realToFrac $ bytesCopied after)          - Bytes (realToFrac $ bytesCopied before)
        !rs_slop        = Bytes (realToFrac $ currentBytesSlop after)     - Bytes (realToFrac $ currentBytesSlop before)
    in RuntimeStats {..}

data RuntimeStats = RuntimeStats
    { rs_cpuElapsed   :: {-# UNPACK #-}!Elapsed
    , rs_cputime      :: {-# UNPACK #-}!CPUTime
    , rs_mutElapsed   :: {-# UNPACK #-}!Elapsed
    , rs_mutTime      :: {-# UNPACK #-}!CPUTime
    , rs_allocated    :: {-# UNPACK #-}!Allocated
    -- , mutated      :: {-# UNPACK #-}!Mutated
    , rs_peak         :: {-# UNPACK #-}!Peak
    , rs_used         :: {-# UNPACK #-}!Used
    , rs_cumulative   :: {-# UNPACK #-}!Cumulative
    , rs_maxBytes     :: {-# UNPACK #-}!Max
    , rs_gcElapsed    :: {-# UNPACK #-}!Elapsed
    , rs_gcTime       :: {-# UNPACK #-}!CPUTime
    , rs_collections  :: {-# UNPACK #-}!Collections
    , rs_uncollected  :: {-# UNPACK #-}!Live
    , rs_copied       :: {-# UNPACK #-}!Copied
    , rs_slop         :: {-# UNPACK #-}!Slop
    } deriving (Generic, Read, Show, Eq, ToJSON, FromJSON)

instance Vary RuntimeStats
instance Similar RuntimeStats
instance Magnitude RuntimeStats

-- instance Pretty RuntimeStats where
--     pretty RuntimeStats {..} =
--         unlines
--             [ ""
--             , divider
--             , header2
--             , divider
--             , cpuTimeStats
--             , mutTimeStats
--             , gcTimeStats
--             , ""
--             , "Collections:" <> pad 11 (pretty collections)
--             , "Leftover:   " <> pad 11 (pretty uncollected)
--             ]
--       where
--         header2 = "                Time           |       Space |    Throughput"
--         divider = "     -------------------------------------------------------"

--         cpuTimeStats =
--           "CPU:"    <> p cputime

--         mutTimeStats =
--           "MUT:"    <> p mutTime
--             <> "  " <> p (mkPercent mutTime cputime)
--             <> "  " <> p allocated
--             <> "  " <> pad 14 (pretty (DataRate allocated mutTime :: SomeDataRate))

--         gcTimeStats =
--           "GC: "    <> p gcTime
--             <> "  " <> p (mkPercent gcTime cputime)
--             <> "  " <> p copied
--             <> "  " <> pad 14 (pretty (DataRate copied gcTime :: SomeDataRate))

--         p :: forall p. Pretty p => p -> String
--         p = pad 12 . pretty

--         pad :: Int -> String -> String
--         pad n s =
--           let l = length s
--           in replicate (n - l) ' ' <> s


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


