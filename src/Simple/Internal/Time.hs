{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simple.Internal.Time where

import GHC.Generics

import Simple.Internal.Improving
import Simple.Internal.Pretty
import Simple.Internal.Similar
import Simple.Internal.Base
import Simple.Internal.Magnitude

import Text.Printf

import Data.Int

newtype Time = Time { getSeconds :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,Read,Show)

instance Pretty Time where
    pretty (Time d)
        | d < 0.001    = printf     "%.0fμs" (d * 1000000)
        | d < 1        = printf     "%.2fms" (d * 1000)
        | d < 60       = printf     "%.2fs"   d
        | d < 60^2     = printf "%.0fm %ds"  (d / 60)   (roundi d `mod` 60)
        | otherwise    = printf "%.0fh %dm"  (d / 60^2) (roundi d `mod` 60^2)
      where
        roundi :: Double -> Int
        roundi = round

pattern PrettyTime :: IsTime t => String -> t
pattern PrettyTime s <- (pretty . fromTime -> s)

class IsTime a where
  toTime :: Time -> a
  fromTime :: a -> Time

instance IsTime Time where
  toTime = id
  fromTime = id

pattern Hours :: IsTime t => Int64 -> t
pattern Hours s <- (round . (/60^2) . getSeconds . fromTime -> s) where
  Hours (toTime . Time . (*60^2) . realToFrac -> s) = s

pattern Minutes :: IsTime t => Int64 -> t
pattern Minutes s <- (round . (/60) . getSeconds . fromTime -> s) where
  Minutes (toTime . Time . (*60) . realToFrac -> s) = s

pattern Seconds :: IsTime t => Int64 -> t
pattern Seconds s <- (round . getSeconds . fromTime -> s) where
  Seconds (toTime . Time . realToFrac -> s) = s

pattern Milliseconds :: IsTime t => Int64 -> t
pattern Milliseconds s <- (round . (* 1000) . getSeconds . fromTime -> s) where
  Milliseconds (toTime . Time . (/1000) . realToFrac -> s) = s

pattern Microseconds :: IsTime t => Int64 -> t
pattern Microseconds s <- (round . (* 1000000) . getSeconds . fromTime -> s) where
  Microseconds (toTime . Time . (/1000000) . realToFrac -> s) = s

pattern Nanoseconds :: IsTime t => Int64 -> t
pattern Nanoseconds s <- (round . (* 1000000000) . getSeconds . fromTime -> s) where
  Nanoseconds (toTime . Time . (/1000000000) . realToFrac -> s) = s

----------------------------------------
-- Elapsed time; wall clock

newtype Elapsed = Elapsed { getElapsed :: Time }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,Read,Show)

instance IsTime Elapsed where
  toTime = Elapsed
  fromTime = getElapsed

instance Improving Elapsed where
  improving old new = old > new -- less time is better
  improvingShow _ = ">"

instance Base Elapsed where
  base (Seconds i) -- yeah, probably not a good idea -- I'm hoping it's intuitive
    | i > 1     = 60
    | otherwise = 10

instance Similar Elapsed where
  similar b (Milliseconds d) (Milliseconds d') = similar b d d'

----------------------------------------
-- CPUTime; cpu clock

newtype CPUTime = CPUTime { getCPUTime :: Time }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,Read,Show)

instance IsTime CPUTime where
  toTime = CPUTime
  fromTime = getCPUTime

instance Improving CPUTime where
  improving old new = old > new
  improvingShow _ = ">"

instance Base CPUTime

instance Similar CPUTime where
  similar b (Microseconds us) (Microseconds us') = similar b us us'
