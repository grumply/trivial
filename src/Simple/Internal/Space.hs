{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simple.Internal.Space where

import Simple.Internal.Improving
import Simple.Internal.Pretty
import Simple.Internal.Similar
import Simple.Internal.Magnitude
import Simple.Internal.Base
import Simple.Internal.Variance

import Data.Int

import GHC.Generics

import Text.Printf

import Data.Aeson

class IsSpace a where
  toSpace :: SomeSpace -> a
  fromSpace :: a -> SomeSpace

newtype SomeSpace = SomeSpace { getBytes :: Double }
  deriving (Generic,Eq,Ord,Num,Real,RealFrac,Fractional,Floating,RealFloat,Read,Show,ToJSON,FromJSON)

instance Vary SomeSpace

instance Base SomeSpace where
  base _ = 2 -- based on my ingrained intuition

instance Similar SomeSpace where -- intuitionistic; megabytes
  sim b (Megabytes b1) (Megabytes b2) = sim b b1 b2

instance Magnitude SomeSpace where -- intuitionistic; megabytes
  mag b (Megabytes b1) (Megabytes b2) = mag b b1 b2

instance IsSpace SomeSpace where
  toSpace = id
  fromSpace = id

pattern Attobytes :: IsSpace b => Double -> b
pattern Attobytes abs <- ((* 1e18) . getBytes . fromSpace -> abs) where
  Attobytes (toSpace . SomeSpace . (/ 1e18) -> s) = s

pattern Femtobytes :: IsSpace b => Double -> b
pattern Femtobytes fbs <- ((* 1e15) . getBytes . fromSpace -> fbs) where
  Femtobytes (toSpace . SomeSpace . (/ 1e15) -> s) = s

pattern Picobytes :: IsSpace b => Double -> b
pattern Picobytes pbs <- ((* 1e12) . getBytes . fromSpace -> pbs) where
  Picobytes (toSpace . SomeSpace . (/ 1e12) -> s) = s

pattern Nanobytes :: IsSpace b => Double -> b
pattern Nanobytes nbs <- ((* 1e9) . getBytes . fromSpace -> nbs) where
  Nanobytes (toSpace . SomeSpace . (/ 1e9) -> s) = s

pattern Microbytes :: IsSpace b => Double -> b
pattern Microbytes ubs <- ((* 1e6) . getBytes . fromSpace -> ubs) where
  Microbytes (toSpace . SomeSpace . (/ 1e6) -> s) = s

pattern Millibytes :: IsSpace b => Double -> b
pattern Millibytes mbs <- ((* 1e3) . getBytes . fromSpace -> mbs) where
  Millibytes (toSpace . SomeSpace . (/ 1e3) -> s) =s

pattern Bytes :: IsSpace b => Double -> b
pattern Bytes bs <- (getBytes . fromSpace -> bs) where
  Bytes (toSpace . SomeSpace -> s) = s

pattern Kibibytes :: IsSpace b => Double -> b
pattern Kibibytes bs <- ((/ 1e3) . getBytes . fromSpace -> bs) where
  Kibibytes (toSpace . SomeSpace . (*1e3) -> s) = s

pattern Kilobytes :: IsSpace b => Double -> b
pattern Kilobytes kbs <- ((/ 2^10) . getBytes . fromSpace -> kbs) where
  Kilobytes (toSpace . SomeSpace . (*2^10) -> s) = s

pattern Mebibytes :: IsSpace b => Double -> b
pattern Mebibytes bs <- ((/ 1e6) . getBytes . fromSpace -> bs) where
  Mebibytes (toSpace . SomeSpace . (*1e6) -> s) = s

pattern Megabytes :: IsSpace b => Double -> b
pattern Megabytes mbs <- ((/ 2^20) . getBytes . fromSpace -> mbs) where
  Megabytes (toSpace . SomeSpace . (*2^20) -> s) = s

pattern Gibibytes :: IsSpace b => Double -> b
pattern Gibibytes bs <- ((/ 1e9) . getBytes . fromSpace -> bs) where
  Gibibytes (toSpace . SomeSpace . (*1e9) -> s) = s

pattern Gigabytes :: IsSpace b => Double -> b
pattern Gigabytes gbs <- ((/ 2^30) . getBytes . fromSpace -> gbs) where
  Gigabytes (toSpace . SomeSpace . (*2^30) -> s) = s

pattern Tebibytes :: IsSpace b => Double -> b
pattern Tebibytes bs <- ((/ 1e12) . getBytes . fromSpace -> bs) where
  Tebibytes (toSpace . SomeSpace . (*1e12) -> s) = s

pattern Terabytes :: IsSpace b => Double -> b
pattern Terabytes tbs <- ((/ 2^40) . getBytes . fromSpace -> tbs) where
  Terabytes (toSpace . SomeSpace . (*2^40) -> s) = s

pattern Pebibytes :: IsSpace b => Double -> b
pattern Pebibytes bs <- ((/ 1e15) . getBytes . fromSpace -> bs) where
  Pebibytes (toSpace . SomeSpace . (*1e15) -> s) = s

pattern Petabytes :: IsSpace b => Double -> b
pattern Petabytes tbs <- ((/ 2^50) . getBytes . fromSpace -> tbs) where
  Petabytes (toSpace . SomeSpace . (*2^50) -> s) = s

pattern Exbibytes :: IsSpace b => Double -> b
pattern Exbibytes bs <- ((/ 1e18) . getBytes . fromSpace -> bs) where
  Exbibytes (toSpace . SomeSpace . (*1e18) -> s) = s

pattern Exabytes :: IsSpace b => Double -> b
pattern Exabytes tbs <- ((/ 2^60) . getBytes . fromSpace -> tbs) where
  Exabytes (toSpace . SomeSpace . (*2^60) -> s) = s

pattern Zebibytes :: IsSpace b => Double -> b
pattern Zebibytes bs <- ((/ 1e21) . getBytes . fromSpace -> bs) where
  Zebibytes (toSpace . SomeSpace . (*1e21) -> s) = s

pattern Zettabytes :: IsSpace b => Double -> b
pattern Zettabytes tbs <- ((/ 2^70) . getBytes . fromSpace -> tbs) where
  Zettabytes (toSpace . SomeSpace . (*2^70) -> s) = s

pattern Yobibytes :: IsSpace b => Double -> b
pattern Yobibytes bs <- ((/ 1e24) . getBytes . fromSpace -> bs) where
  Yobibytes (toSpace . SomeSpace . (*1e24) -> s) = s

pattern Yottabytes :: IsSpace b => Double -> b
pattern Yottabytes tbs <- ((/ 2^80) . getBytes . fromSpace -> tbs) where
  Yottabytes (toSpace . SomeSpace . (*2^80) -> s) = s

instance Pretty SomeSpace where
    pretty (SomeSpace b)
        | b < 1e-15 = printf "%.2f aB"  (b * 1e18)
        | b < 1e-12 = printf "%.2f fB"  (b * 1e15)
        | b < 1e-9  = printf "%.2f pB"  (b * 1e12)
        | b < 1e-6  = printf "%.2f nB"  (b * 1e9)
        | b < 1e-3  = printf "%.2f μB"  (b * 1e6)
        | b < 1     = printf "%.2f mB"  (b * 1e3)
        | b < 1e3   = printf "%.0f B"    b
        | b < 1e6   = printf "%.2f KiB" (b / 1e3)
        | b < 1e9   = printf "%.2f MiB" (b / 1e6)
        | b < 1e12  = printf "%.2f GiB" (b / 1e9)
        | b < 1e15  = printf "%.2f TiB" (b / 1e12)
        | b < 1e18  = printf "%.2f PiB" (b / 1e15)
        | b < 1e21  = printf "%.2f EiB" (b / 1e18)
        | b < 1e24  = printf "%.2f ZiB" (b / 1e21)
        | otherwise = printf "%.2f YiB" (b / 1e24)

----------------------------------------
-- Mutated Bytes

newtype Mutated = Mutated { getMutated :: SomeSpace }
  deriving (Generic,Eq,Ord,Num,Real,RealFrac,Floating,RealFloat,Fractional,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance Vary Mutated

instance IsSpace Mutated where
  toSpace = Mutated
  fromSpace = getMutated

instance Similar Mutated where
  sim b (Megabytes mb1) (Megabytes mb2) = sim b mb1 mb2

instance Improving Mutated where
  improving b1 b2 = b1 > b2 -- fewer mutated bytes are better
  improvingShow _ = ">"

----------------------------------------
-- Allocated Bytes

newtype Allocated = Allocated { getAllocated :: SomeSpace }
  deriving (Generic,Eq,Ord,Num,Real,RealFrac,Floating,RealFloat,Fractional,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance Vary Allocated

instance IsSpace Allocated where
  toSpace = Allocated
  fromSpace = getAllocated

instance Similar Allocated where
  sim b (Megabytes mb1) (Megabytes mb2) = sim b mb1 mb2

instance Improving Allocated where
  improving b1 b2 = b1 > b2 -- fewer mutated bytes are better
  improvingShow _ = ">"

----------------------------------------
-- De-Allocated Bytes

newtype Deallocated = Deallocated { getDeallocated :: SomeSpace }
  deriving (Generic,Eq,Ord,Num,Real,RealFrac,Floating,RealFloat,Fractional,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance Vary Deallocated

instance IsSpace Deallocated where
  toSpace = Deallocated
  fromSpace = getDeallocated

instance Similar Deallocated where
  sim b (Megabytes mb1) (Megabytes mb2) = sim b mb1 mb2

instance Improving Deallocated where
  improving b1 b2 = b1 > b2 -- fewer mutated bytes are better
  improvingShow _ = ">"

----------------------------------------
-- GC Slop

newtype Slop = Slop { getSlop :: SomeSpace }
  deriving (Generic,Eq,Ord,Num,Real,RealFrac,Floating,RealFloat,Fractional,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance Vary Slop

instance IsSpace Slop where
  toSpace = Slop
  fromSpace = getSlop

instance Similar Slop where
  sim b (Bytes b1) (Bytes b2) = sim b b1 b2

instance Improving Slop where
  improving b1 b2 = b1 > b2 -- fewer mutated bytes are better
  improvingShow _ = ">"

----------------------------------------
-- Max Bytes

newtype Max = Max { getMax :: SomeSpace }
  deriving (Generic,Eq,Ord,Num,Real,RealFrac,Floating,RealFloat,Fractional,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance Vary Max

instance IsSpace Max where
  toSpace = Max
  fromSpace = getMax

instance Similar Max where
  sim b (Bytes b1) (Bytes b2) = sim b b1 b2

instance Improving Max where
  improving b1 b2 = b1 > b2 -- fewer mutated bytes are better
  improvingShow _ = ">"

----------------------------------------
-- Cumulative Bytes

newtype Cumulative = Cumulative { getCumulative :: SomeSpace }
  deriving (Generic,Eq,Ord,Num,Real,RealFrac,Floating,RealFloat,Fractional,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance Vary Cumulative

instance IsSpace Cumulative where
  toSpace = Cumulative
  fromSpace = getCumulative

instance Similar Cumulative where
  sim b (Bytes b1) (Bytes b2) = sim b b1 b2

instance Improving Cumulative where
  improving b1 b2 = b1 > b2 -- fewer mutated bytes are better
  improvingShow _ = ">"

----------------------------------------
-- Copied Bytes

newtype Copied = Copied { getCopied :: SomeSpace }
  deriving (Generic,Eq,Ord,Num,Real,RealFrac,Floating,RealFloat,Fractional,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance Vary Copied

instance IsSpace Copied where
  toSpace = Copied
  fromSpace = getCopied

instance Similar Copied where
  sim b (Bytes b1) (Bytes b2) = sim b b1 b2

instance Improving Copied where
  improving b1 b2 = b1 > b2 -- fewer mutated bytes are better
  improvingShow _ = ">"

----------------------------------------
-- Bytes Used

newtype Used = Used { getUsed :: SomeSpace }
  deriving (Generic,Eq,Ord,Num,Real,RealFrac,Floating,RealFloat,Fractional,Read,Show,Pretty,ToJSON,FromJSON,Magnitude,Base)

instance Vary Used

instance IsSpace Used where
  toSpace = Used
  fromSpace = getUsed

instance Similar Used where
  sim b (Bytes b1) (Bytes b2) = sim b b1 b2

instance Improving Used where
  improving b1 b2 = b1 > b2 -- fewer mutated bytes are better
  improvingShow _ = ">"

----------------------------------------
-- Max Slop

newtype MaxSlop = MaxSlop { getMaxSlop :: SomeSpace }
  deriving (Generic,Eq,Ord,Num,Real,RealFrac,Floating,RealFloat,Fractional,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance Vary MaxSlop

instance IsSpace MaxSlop where
  toSpace = MaxSlop
  fromSpace = getMaxSlop

instance Similar MaxSlop where
  sim b (Bytes b1) (Bytes b2) = sim b b1 b2

instance Improving MaxSlop where
  improving b1 b2 = b1 > b2 -- fewer mutated bytes are better
  improvingShow _ = ">"

----------------------------------------
-- Peak Allocated

newtype Peak = Peak { getPeak :: SomeSpace }
  deriving (Generic,Eq,Ord,Num,Real,RealFrac,Floating,RealFloat,Fractional,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance Vary Peak

instance IsSpace Peak where
  toSpace = Peak
  fromSpace = getPeak

instance Similar Peak where
  sim b (Bytes b1) (Bytes b2) = sim b b1 b2

instance Improving Peak where
  improving b1 b2 = b1 > b2 -- fewer mutated bytes are better
  improvingShow _ = ">"

----------------------------------------
-- Live Bytes

newtype Live = Live { getLive :: SomeSpace }
  deriving (Generic,Eq,Ord,Num,Real,RealFrac,Floating,RealFloat,Fractional,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance Vary Live

instance IsSpace Live where
  toSpace = Live
  fromSpace = getLive

instance Similar Live where
  sim b (Bytes b1) (Bytes b2) = sim b b1 b2

instance Improving Live where
  improving b1 b2 = b1 > b2 -- fewer mutated bytes are better
  improvingShow _ = ">"

