{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Simple.Internal.DataRate where

import Simple.Internal.Improving
import Simple.Internal.Pretty
import Simple.Internal.Similar
import Simple.Internal.Magnitude
import Simple.Internal.Base
import Simple.Internal.Space
import Simple.Internal.Time
import Simple.Internal.Variance

import Data.Int

import GHC.Generics

import Text.Printf

import Data.Aeson

class IsDataRate r where
  toDataRate :: SomeDataRate -> r
  fromDataRate :: r -> SomeDataRate

instance IsDataRate Double where
  toDataRate = getDataRate
  fromDataRate = SomeDataRate

newtype SomeDataRate = SomeDataRate { getDataRate :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,RealFrac,RealFloat,Read,Show,ToJSON,FromJSON)

instance Vary SomeDataRate

instance Improving SomeDataRate

instance Magnitude SomeDataRate

instance Similar SomeDataRate

instance Pretty SomeDataRate where
    pretty (SomeDataRate t)
        | isNaN t   = "0 B/s"
        | t < 2^10  = printf "%.0f B/s"  t
        | t < 2^20  = printf "%.1f KB/s" (t / 2^10)
        | t < 2^30  = printf "%.1f MB/s" (t / 2^20)
        | t < 2^40  = printf "%.1f GB/s" (t / 2^30)
        | otherwise = printf "%.1f TB/s" (t / 2^40)

instance IsDataRate SomeDataRate where
  toDataRate = id
  fromDataRate = id

viewDataRate :: (IsDataRate r, IsSpace s, IsTime t) => r -> (s,t)
viewDataRate (fromDataRate -> SomeDataRate r) = (Bytes r,Seconds 1)

mkDataRate :: (IsDataRate r, IsSpace s, IsTime t) => s -> t -> r
mkDataRate s t =
  let time = realToFrac (fromTime t)
  in toDataRate $
       if time == 0 then
         SomeDataRate 0
       else
         SomeDataRate (realToFrac (fromSpace s) / time)

pattern DataRate :: (IsSpace s, IsTime t, IsDataRate r) => s -> t -> r
pattern DataRate s t <- (viewDataRate -> (s,t)) where
  DataRate s t = mkDataRate s t

----------------------------------------
-- GC copy rate

newtype CopyRate = CopyRate { getCopyRate :: SomeDataRate }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,RealFrac,RealFloat,Read,Show,ToJSON,FromJSON,Pretty)

instance Vary CopyRate

instance IsDataRate CopyRate where
  toDataRate = CopyRate
  fromDataRate = getCopyRate

instance Improving CopyRate -- more throughput is better

instance Magnitude CopyRate

instance Base CopyRate where
  base _ = 2

instance Similar CopyRate where
  sim b (DataRate (Megabytes mbps :: SomeSpace) (_ :: SomeTime))
            (DataRate (Megabytes mbps':: SomeSpace) (_ :: SomeTime))
    = sim b mbps mbps'

----------------------------------------
-- Allocation Rate

newtype AllocationRate = AllocationRate { getAllocationRate :: SomeDataRate }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,RealFrac,RealFloat,Read,Show,ToJSON,FromJSON,Pretty)

instance Vary AllocationRate

instance Magnitude AllocationRate

instance IsDataRate AllocationRate where
  toDataRate = AllocationRate
  fromDataRate = getAllocationRate

instance Improving AllocationRate -- more throughput is better

instance Base AllocationRate where
  base _ = 2

instance Similar AllocationRate where
  sim b
    (DataRate (Megabytes mbps :: SomeSpace) (_ :: SomeTime))
    (DataRate (Megabytes mbps' :: SomeSpace) (_ :: SomeTime))
      = sim b mbps mbps'

----------------------------------------
-- Deallocation Rate

newtype DeallocationRate = DeallocationRate { getDeallocationRate :: SomeDataRate }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,RealFrac,RealFloat,Read,Show,ToJSON,FromJSON,Pretty)

instance Vary DeallocationRate

instance Magnitude DeallocationRate

instance IsDataRate DeallocationRate where
  toDataRate = DeallocationRate
  fromDataRate = getDeallocationRate

instance Improving DeallocationRate -- more throughput is better

instance Base DeallocationRate where
  base _ = 2

instance Similar DeallocationRate where
  sim b
    (DataRate (Megabytes mbps :: SomeSpace) (_ :: SomeTime))
    (DataRate (Megabytes mbps' :: SomeSpace) (_ :: SomeTime))
      = sim b mbps mbps'

