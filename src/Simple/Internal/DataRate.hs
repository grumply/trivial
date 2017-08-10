{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simple.Internal.DataRate where

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

import Data.Aeson

class IsDataRate r where
  toDataRate :: DataRate -> r
  fromDataRate :: r -> DataRate

newtype DataRate = DataRate { getDataRate :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,ToJSON,FromJSON)

instance Pretty DataRate where
    pretty (DataRate t)
        | isNaN t   = "0 B/s"
        | t < 2^10  = printf "%.0f B/s"  t
        | t < 2^20  = printf "%.1f KB/s" (t / 2^10)
        | t < 2^30  = printf "%.1f MB/s" (t / 2^20)
        | t < 2^40  = printf "%.1f GB/s" (t / 2^30)
        | otherwise = printf "%.1f TB/s" (t / 2^40)

instance IsDataRate DataRate where
  toDataRate = id
  fromDataRate = id

viewDataRate :: (IsSpace s, IsTime t) => DataRate -> (s,t)
viewDataRate (DataRate r) = (Bytes (round r),Seconds 1)

mkDataRate :: (Real a, Real b) => a -> b -> DataRate
mkDataRate b t = DataRate (realToFrac b / realToFrac t)

pattern Throughput :: (IsSpace s, IsTime t, IsDataRate r) => s -> t -> r
pattern Throughput s t <- (viewDataRate . fromDataRate -> (s,t)) where
  Throughput (Bytes s) (Seconds t) = toDataRate $ mkDataRate s t

pattern Bps :: IsDataRate r => Double -> r
pattern Bps bps <- (getDataRate . fromDataRate -> bps) where
  Bps (toDataRate . DataRate -> r) = r

pattern KBps :: IsDataRate r => Double -> r
pattern KBps kbps <- ((/ 2^10) . getDataRate . fromDataRate -> kbps) where
  KBps (toDataRate . DataRate . (*2^10) -> r) = r

pattern MBps :: IsDataRate r => Double -> r
pattern MBps mbps <- ((/ 2^20) . getDataRate . fromDataRate -> mbps) where
  MBps (toDataRate . DataRate . (*2^20) -> r) = r

pattern GBps :: IsDataRate r => Double -> r
pattern GBps gbps <- ((/ 2^30) . getDataRate . fromDataRate -> gbps) where
  GBps (toDataRate . DataRate . (*2^30) -> r) = r

pattern TBps :: IsDataRate r => Double -> r
pattern TBps tbps <- ((/ 2^40) . getDataRate . fromDataRate -> tbps) where
  TBps (toDataRate . DataRate . (*2^40) -> r) = r

----------------------------------------
-- GC copy rate

newtype CopyRate = CopyRate { getCopyRate :: DataRate }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,ToJSON,FromJSON)

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
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,ToJSON,FromJSON)

instance IsDataRate AllocationRate where
  toDataRate = AllocationRate
  fromDataRate = getAllocationRate

instance Improving AllocationRate -- more throughput is better

instance Base AllocationRate where
  base _ = 2

instance Similar AllocationRate where
  similar b (MBps mbps) (MBps mbps') = similar b mbps mbps'

