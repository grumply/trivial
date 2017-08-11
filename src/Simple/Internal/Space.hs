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

import Data.Int

import GHC.Generics

import Text.Printf

import Data.Aeson

class IsSpace a where
  toSpace :: Space -> a
  fromSpace :: a -> Space

newtype Space = Space { getBytes :: Int64 }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,ToJSON,FromJSON)

instance Base Space where
  base _ = 2

instance Similar Space where
  similar b (Megabytes b1) (Megabytes b2) = similar b b1 b2

instance Magnitude Space where
  mag b (Megabytes b1) (Megabytes b2) = mag b b1 b2

instance IsSpace Space where
  toSpace = id
  fromSpace = id

pattern Bytes :: IsSpace b => Int64 -> b
pattern Bytes bs <- (getBytes . fromSpace -> bs) where
  Bytes (toSpace . Space -> s) = s

pattern Kilobytes :: IsSpace b => Int64 -> b
pattern Kilobytes kbs <- ((`div` 2^10) . getBytes . fromSpace -> kbs) where
  Kilobytes (toSpace . Space . (*2^10) -> s) = s

pattern Megabytes :: IsSpace b => Int64 -> b
pattern Megabytes mbs <- ((`div` 2^20) . getBytes . fromSpace -> mbs) where
  Megabytes (toSpace . Space . (*2^20) -> s) = s

pattern Gigabytes :: IsSpace b => Int64 -> b
pattern Gigabytes gbs <- ((`div` 2^30) . getBytes . fromSpace -> gbs) where
  Gigabytes (toSpace . Space . (*2^30) -> s) = s

pattern Terabytes :: IsSpace b => Int64 -> b
pattern Terabytes tbs <- ((`div` 2^40) . getBytes . fromSpace -> tbs) where
  Terabytes (toSpace . Space . (*2^40) -> s) = s

instance Pretty Space where
    pretty (Space b)
        | b < 2^10  = printf "%d B" b
        | b < 2^20  = printf "%.1f KB" (realToFrac b / 2^10 :: Double)
        | b < 2^30  = printf "%.1f MB" (realToFrac b / 2^20 :: Double)
        | b < 2^40  = printf "%.1f GB" (realToFrac b / 2^30 :: Double)
        | otherwise = printf "%.1f TB" (realToFrac b / 2^40 :: Double)

----------------------------------------
-- Mutated Bytes

newtype Mutated = Mutated { getMutated :: Space }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance IsSpace Mutated where
  toSpace = Mutated
  fromSpace = getMutated

instance Similar Mutated where
  similar b (Megabytes mb1) (Megabytes mb2) = similar b mb1 mb2

instance Improving Mutated where
  improving b1 b2 = b1 > b2 -- fewer mutated bytes are better
  improvingShow _ = ">"

----------------------------------------
-- Allocated Bytes

newtype Allocated = Allocated { getAllocated :: Space }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance IsSpace Allocated where
  toSpace = Allocated
  fromSpace = getAllocated

instance Similar Allocated where
  similar b (Megabytes mb1) (Megabytes mb2) = similar b mb1 mb2

instance Improving Allocated where
  improving b1 b2 = b1 > b2 -- fewer mutated bytes are better

----------------------------------------
-- GC Slop

newtype Slop = Slop { getSlop :: Space }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance IsSpace Slop where
  toSpace = Slop
  fromSpace = getSlop

instance Similar Slop where
  similar b (Bytes b1) (Bytes b2) = similar b b1 b2

instance Improving Slop where
  improving b1 b2 = b1 > b2 -- fewer mutated bytes are better
  improvingShow _ = ">"

----------------------------------------
-- Max Bytes

newtype Max = Max { getMax :: Space }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance IsSpace Max where
  toSpace = Max
  fromSpace = getMax

instance Similar Max where
  similar b (Bytes b1) (Bytes b2) = similar b b1 b2

instance Improving Max where
  improving b1 b2 = b1 > b2 -- fewer mutated bytes are better
  improvingShow _ = ">"

----------------------------------------
-- Cumulative Bytes

newtype Cumulative = Cumulative { getCumulative :: Space }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance IsSpace Cumulative where
  toSpace = Cumulative
  fromSpace = getCumulative

instance Similar Cumulative where
  similar b (Bytes b1) (Bytes b2) = similar b b1 b2

instance Improving Cumulative where
  improving b1 b2 = b1 > b2 -- fewer mutated bytes are better
  improvingShow _ = ">"

----------------------------------------
-- Copied Bytes

newtype Copied = Copied { getCopied :: Space }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance IsSpace Copied where
  toSpace = Copied
  fromSpace = getCopied

instance Similar Copied where
  similar b (Bytes b1) (Bytes b2) = similar b b1 b2

instance Improving Copied where
  improving b1 b2 = b1 > b2 -- fewer mutated bytes are better
  improvingShow _ = ">"

----------------------------------------
-- Bytes Used

newtype Used = Used { getUsed :: Space }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,Pretty,ToJSON,FromJSON,Magnitude,Base)

instance IsSpace Used where
  toSpace = Used
  fromSpace = getUsed

instance Similar Used where
  similar b (Bytes b1) (Bytes b2) = similar b b1 b2

instance Improving Used where
  improving b1 b2 = b1 > b2 -- fewer mutated bytes are better
  improvingShow _ = ">"

----------------------------------------
-- Max Slop

newtype MaxSlop = MaxSlop { getMaxSlop :: Space }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance IsSpace MaxSlop where
  toSpace = MaxSlop
  fromSpace = getMaxSlop

instance Similar MaxSlop where
  similar b (Bytes b1) (Bytes b2) = similar b b1 b2

instance Improving MaxSlop where
  improving b1 b2 = b1 > b2 -- fewer mutated bytes are better
  improvingShow _ = ">"

----------------------------------------
-- Peak Allocated

newtype Peak = Peak { getPeak :: Space }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance IsSpace Peak where
  toSpace = Peak
  fromSpace = getPeak

instance Similar Peak where
  similar b (Bytes b1) (Bytes b2) = similar b b1 b2

instance Improving Peak where
  improving b1 b2 = b1 > b2 -- fewer mutated bytes are better
  improvingShow _ = ">"

----------------------------------------
-- Live Bytes

newtype Live = Live { getLive :: Space }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance IsSpace Live where
  toSpace = Live
  fromSpace = getLive

instance Similar Live where
  similar b (Bytes b1) (Bytes b2) = similar b b1 b2

instance Improving Live where
  improving b1 b2 = b1 > b2 -- fewer mutated bytes are better
  improvingShow _ = ">"

