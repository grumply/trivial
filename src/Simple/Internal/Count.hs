{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simple.Internal.Count where

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

newtype Count = Count { getCount :: Int64 }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,Integral,Enum,Base,Similar)


class IsCount c where
  toCount :: Count -> c
  fromCount :: c -> Count

instance IsCount Count where
  toCount = id
  fromCount = id

newtype ByteUsageSamples = ByteUsageSamples { getByteUsageSamples :: Count }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,Integral,Enum,Base,Similar)

instance IsCount ByteUsageSamples where
  toCount = ByteUsageSamples
  fromCount (ByteUsageSamples bus) = bus

----------------------------------------
-- Collection Count

newtype Collections = Collections { getCollections :: Count }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show)

instance IsCount Collections where
  toCount = Collections
  fromCount = getCollections

instance Pretty Collections where
  pretty (Collections c) = show c

instance Improving Collections where
  improving c1 c2 = c1 > c2
  improvingShow _ = ">"

instance Similar Collections

instance Base Collections
