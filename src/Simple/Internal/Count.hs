{-# LANGUAGE ViewPatterns #-}
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
import Simple.Internal.Variance

import Data.Int

import GHC.Generics

import Text.Printf

import Data.Aeson

newtype SomeCount = SomeCount { getCount :: Int64 }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,Integral,Enum,Base,Similar,ToJSON,FromJSON,Magnitude)

instance Vary SomeCount

pattern Count :: IsCount c => Int64 -> c
pattern Count c <- (getCount . fromCount -> c) where
  Count c = toCount (SomeCount c)

instance Pretty SomeCount where
  pretty (SomeCount c) = show c

instance IsCount Int64 where
  toCount (SomeCount c) = c
  fromCount = SomeCount

class IsCount c where
  toCount :: SomeCount -> c
  fromCount :: c -> SomeCount

instance IsCount SomeCount where
  toCount = id
  fromCount = id

newtype ByteUsageSamples = ByteUsageSamples { getByteUsageSamples :: SomeCount }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,Integral,Enum,Base,Similar,ToJSON,FromJSON)

instance IsCount ByteUsageSamples where
  toCount = ByteUsageSamples
  fromCount (ByteUsageSamples bus) = bus

----------------------------------------
-- Collection Count

newtype Collections = Collections { getCollections :: SomeCount }
  deriving (Generic,Eq,Ord,Num,Real,Enum,Integral,Read,Show,ToJSON,FromJSON,Base,Magnitude,Similar)

instance Vary Collections

instance IsCount Collections where
  toCount = Collections
  fromCount = getCollections

instance Pretty Collections where
  pretty (Collections c) = pretty c

instance Improving Collections where
  improving c1 c2 = c1 > c2
  improvingShow _ = ">"
