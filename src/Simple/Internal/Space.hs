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
  base _ = 2

instance Similar SomeSpace where
  sim b (Megabytes b1) (Megabytes b2) = sim b b1 b2

instance Magnitude SomeSpace where
  mag b (Megabytes b1) (Megabytes b2) = mag b b1 b2

instance IsSpace SomeSpace where
  toSpace = id
  fromSpace = id

pattern Bytes :: IsSpace b => Double -> b
pattern Bytes bs <- (getBytes . fromSpace -> bs) where
  Bytes (toSpace . SomeSpace -> s) = s

pattern Kilobytes :: IsSpace b => Double -> b
pattern Kilobytes kbs <- ((/ 2^10) . getBytes . fromSpace -> kbs) where
  Kilobytes (toSpace . SomeSpace . (*2^10) -> s) = s

pattern Megabytes :: IsSpace b => Double -> b
pattern Megabytes mbs <- ((/ 2^20) . getBytes . fromSpace -> mbs) where
  Megabytes (toSpace . SomeSpace . (*2^20) -> s) = s

pattern Gigabytes :: IsSpace b => Double -> b
pattern Gigabytes gbs <- ((/ 2^30) . getBytes . fromSpace -> gbs) where
  Gigabytes (toSpace . SomeSpace . (*2^30) -> s) = s

pattern Terabytes :: IsSpace b => Double -> b
pattern Terabytes tbs <- ((/ 2^40) . getBytes . fromSpace -> tbs) where
  Terabytes (toSpace . SomeSpace . (*2^40) -> s) = s

instance Pretty SomeSpace where
    pretty (SomeSpace b)
        | b < 2**(-40) = printf "%.2f aB" (b * 2^50)
        | b < 2**(-30) = printf "%.2f fB" (b * 2^40)
        | b < 2**(-20) = printf "%.2f pB" (b * 2^30)
        | b < 2**(-20) = printf "%.2f nB" (b * 2^30)
        | b < 2**(-10) = printf "%.2f μB" (b * 2^20)
        | b < 1        = printf "%.2f mB" (b * 2^10)
        | b < 2^10     = printf "%.0f B" b
        | b < 2^20     = printf "%.2f KB" (b / 2^10)
        | b < 2^30     = printf "%.2f MB" (b / 2^20)
        | b < 2^40     = printf "%.2f GB" (b / 2^30)
        | otherwise    = printf "%.2f TB" (b / 2^40)

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

