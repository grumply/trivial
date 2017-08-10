{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simple.Internal.Rate where

import Simple.Internal.Improving
import Simple.Internal.Pretty
import Simple.Internal.Similar
import Simple.Internal.Magnitude
import Simple.Internal.Base
import Simple.Internal.Space
import Simple.Internal.Time

import Data.Int

import GHC.Generics

import Text.Printf

class IsDataRate r where
  toDataRate :: DataRate -> r
  fromDataRate :: r -> DataRate

newtype DataRate = DataRate { getDataRate :: Int64 }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,Integral,Enum)

instance Pretty DataRate where
    pretty (DataRate t)
        | t < 2^10  = printf "%d B/s"  t
        | t < 2^20  = printf "%.1f KB/s" (t `div` 2^10)
        | t < 2^30  = printf "%.1f MB/s" (t `div` 2^20)
        | t < 2^40  = printf "%.1f GB/s" (t `div` 2^30)
        | otherwise = printf "%.1f TB/s" (t `div` 2^40)

instance IsDataRate DataRate where
  toDataRate = id
  fromDataRate = id

viewDataRate :: (IsSpace s, IsTime t) => DataRate -> (s,t)
viewDataRate (DataRate r) = (Bytes r,Seconds 1)

mkDataRate :: Int64 -> Int64 -> DataRate
mkDataRate b t = DataRate (b `div` t)

pattern ViewByteRate :: (IsSpace s, IsTime t, IsRate r) => s -> t -> r
pattern ViewByteRate s t <- (viewRate . fromDataRate -> (s,t)) where
  DataRate (Bytes s) (Seconds t) = toDataRate $ mkRate s t

pattern Bps :: IsRate r => Int64 -> r
pattern Bps bps <- (getRate . fromDataRate -> bps) where
  Bps (toDataRate . Rate -> r) = r

pattern KBps :: IsRate r => Int64 -> r
pattern KBps kbps <- ((`div` 2^10) . getRate . fromDataRate -> kbps) where
  KBps (toDataRate . Rate . (*2^10) -> r) = r

pattern MBps :: IsRate r => Int64 -> r
pattern MBps mbps <- ((`div` 2^20) . getRate . fromDataRate -> mbps) where
  MBps (toDataRate . Rate . (*2^20) -> r) = r

pattern GBps :: IsRate r => Int64 -> r
pattern GBps gbps <- ((`div` 2^30) . getRate . fromDataRate -> gbps) where
  GBps (toDataRate . Rate . (*2^30) -> r) = r

pattern TBps :: IsRate r => Int64 -> r
pattern TBps tbps <- ((`div` 2^40) . getRate . fromDataRate -> tbps) where
  TBps (toDataRate . Rate . (*2^40) -> r) = r

----------------------------------------
-- GC copy rate

newtype CopyRate = CopyRate { getCopyRate :: DataRate }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,Integral,Enum)

instance IsDataRate CopyRate where
  toDataRate = CopyRate
  fromDataRate = getCopyRate

instance Improving CopyRate -- more throughput is better

instance Base CopyRate where
  base _ = 2

instance Similar CopyRate where
  similar b (MBps mbps) (MBps mbps') = similar b mbps mbps'

----------------------------------------
-- Allocation Rate

newtype AllocationRate = AllocationRate { getAllocationRate :: DataRate }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,Integral,Enum)

instance IsDataRate AllocationRate where
  toDataRate = AllocationRate
  fromDataRate = getAllocationRate

instance Improving AllocationRate -- more throughput is better

instance Base AllocationRate where
  base _ = 2

instance Similar AllocationRate where
  similar b (MBps mbps) (MBps mbps') = similar b mbps mbps'

