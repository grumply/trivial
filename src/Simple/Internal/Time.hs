{-# LANGUAGE ViewPatterns #-}
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

import Simple.Internal.Variance

import Text.Printf

import Data.Int

import Data.Aeson

newtype Time = Time { getSeconds :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,Read,Show,ToJSON,FromJSON)
instance Vary Time
instance Base Time
instance Magnitude Time

instance Pretty Time where
    pretty (Time d)
        | d < 0.000000001 = printf     "%.0fps" (d * 1000000000000)
        | d < 0.000001    = printf     "%.1fns" (d * 1000000000)
        | d < 0.001       = printf     "%.1fμs" (d * 1000000)
        | d < 1           = printf     "%.2fms" (d * 1000)
        | d < 60          = printf     "%.2fs"   d
        | d < 60^2        = printf "%.0fm %ds"  (d / 60)   (roundi d `mod` 60)
        | otherwise       = printf "%.0fh %dm"  (d / 60^2) (roundi d `mod` 60^2)
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

pattern Hours :: IsTime t => Double -> t
pattern Hours s <- ((/60^2) . getSeconds . fromTime -> s) where
  Hours (toTime . Time . (*60^2) -> s) = s

pattern Minutes :: IsTime t => Double -> t
pattern Minutes s <- ((/60) . getSeconds . fromTime -> s) where
  Minutes (toTime . Time . (*60) -> s) = s

pattern Seconds :: IsTime t => Double -> t
pattern Seconds s <- (getSeconds . fromTime -> s) where
  Seconds (toTime . Time -> s) = s

pattern Milliseconds :: IsTime t => Double -> t
pattern Milliseconds s <- ((* 1000) . getSeconds . fromTime -> s) where
  Milliseconds (toTime . Time . (/1000) -> s) = s

pattern Microseconds :: IsTime t => Double -> t
pattern Microseconds s <- ((* 1000000) . getSeconds . fromTime -> s) where
  Microseconds (toTime . Time . (/1000000) -> s) = s

pattern Nanoseconds :: IsTime t => Double -> t
pattern Nanoseconds s <- ((* 1000000000) . getSeconds . fromTime -> s) where
  Nanoseconds (toTime . Time . (/1000000000) -> s) = s

pattern Picoseconds :: IsTime t => Double -> t
pattern Picoseconds s <- ((* 1000000000000) . getSeconds . fromTime -> s) where
  Picoseconds (toTime . Time . (/1000000000000) -> s) = s


----------------------------------------
-- Elapsed time; wall clock

newtype Elapsed = Elapsed Time
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,Read,Show,ToJSON,FromJSON,Pretty,Magnitude)

instance Vary Elapsed

instance IsTime Elapsed where
  toTime = Elapsed
  fromTime (Elapsed e) = e

instance Improving Elapsed where
  improving old new = old > new -- less time is better
  improvingShow _ = ">"

instance Base Elapsed where
  base (Seconds i) -- yeah, probably not a good idea -- I'm hoping it's intuitive
    | i > 1     = 60
    | otherwise = 10

instance Similar Elapsed where
  sim b (Milliseconds d) (Milliseconds d') = sim b d d'

class HasElapsed a where
  elapsed :: a -> Elapsed

----------------------------------------
-- CPUTime; cpu clock

newtype CPUTime = CPUTime Time
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,Read,Show,ToJSON,FromJSON,Pretty,Magnitude)

instance Vary CPUTime

instance IsTime CPUTime where
  toTime = CPUTime
  fromTime (CPUTime cput) = cput

instance Improving CPUTime where
  improving old new = old > new
  improvingShow _ = ">"

instance Base CPUTime

instance Similar CPUTime where
  sim b (Microseconds us) (Microseconds us') = sim b us us'

class HasCPUTime a where
  cpuTime :: a -> CPUTime
