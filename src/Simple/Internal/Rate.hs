{-# LANGUAGE ViewPatterns #-}
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
import Simple.Internal.Variance

import Data.Int

import GHC.Generics

import Text.Printf

import Data.Aeson

class IsRate r where
  toRate :: PerSecond -> r
  fromRate :: r -> PerSecond

newtype PerSecond = PerSecond { getPerSecond :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,RealFrac,RealFloat,Read,Show,ToJSON,FromJSON)

instance Vary PerSecond
instance Similar PerSecond
instance Magnitude PerSecond
instance Improving PerSecond

instance  Pretty PerSecond where
    pretty (PerSecond r) = printf "%.2f/s" r

instance IsRate PerSecond where
    toRate = id
    fromRate = id

viewPerSecond :: IsTime t => PerSecond -> (Double,t)
viewPerSecond (PerSecond r) = (r,Seconds 1)

mkPerSecond :: (Real a, Real b) => a -> b -> PerSecond
mkPerSecond b t =
  let time = realToFrac t
  in if time == 0 then
       PerSecond 0
     else
       PerSecond (realToFrac b / time)

pattern Rate :: (IsTime t, IsRate r) => Double -> t -> r
pattern Rate d t <- (viewPerSecond . fromRate -> (d,t)) where
  Rate d (Seconds t) = toRate $ mkPerSecond d t

----------------------------------------
-- Collection Rate

newtype CollectionRate = CollectionRate { getCollectionRate :: PerSecond }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,RealFrac,RealFloat,Read,Show,ToJSON,FromJSON)

instance Vary CollectionRate
instance Similar CollectionRate
instance Magnitude CollectionRate

instance IsRate CollectionRate where
  toRate = CollectionRate
  fromRate (CollectionRate ps) = ps

instance Improving CollectionRate where
  improving = (>)
  improvingShow _ = ">"

instance Base CollectionRate

instance Pretty CollectionRate where
  pretty (CollectionRate (PerSecond c)) = printf "%d collections/s" c
