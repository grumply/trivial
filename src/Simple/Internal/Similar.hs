{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Simple.Internal.Similar where

import Data.Int
import Data.Word

import GHC.Generics

class Similar a where
  sim :: (Real b) => b -> a -> a -> Bool
  default sim :: (Generic a,GSimilar (Rep a),Real b) => b -> a -> a -> Bool
  sim b x y = gsim b (from x) (from y)

similarReals :: (Eq a, Real a, Real b) => b -> a -> a -> Bool
similarReals _ 0 0 = True
similarReals (realToFrac -> b) (realToFrac -> x) (realToFrac -> y) =
      -- As b ↘ 1, epsilon ↘ 0
      let epsilon = (x :: Double) / (logBase b (x + 1) + 1)
          (lo,hi) = (x - epsilon,x + epsilon)
      in lo <= y && y <= hi

similar :: (Similar a) => a -> a -> Bool
similar = sim (exp 1)

instance Similar Double where sim = similarReals
instance Similar Float where sim = similarReals
instance Similar Int where sim = similarReals
instance Similar Integer where sim = similarReals
instance Similar Int64 where sim = similarReals
instance Similar Int32 where sim = similarReals
instance Similar Int16 where sim = similarReals
instance Similar Int8 where sim = similarReals
instance Similar Word64 where sim = similarReals
instance Similar Word32 where sim = similarReals
instance Similar Word16 where sim = similarReals
instance Similar Word8 where sim = similarReals

class GSimilar a where
  gsim :: (Real b) => b -> a x -> a x -> Bool

instance ( Datatype d, GSimilar a ) => GSimilar ( D1 d a ) where
  gsim b (M1 d1) (M1 d2) = gsim b d1 d2

instance ( Constructor c, GSimilar a ) => GSimilar ( C1 c a ) where
  gsim b (M1 c1) (M1 c2) = gsim b c1 c2

instance ( Selector s, GSimilar a ) => GSimilar ( S1 s a ) where
  gsim b (M1 s1) (M1 s2) = gsim b s1 s2

instance ( Similar a ) => GSimilar ( K1 r a ) where
  gsim b (K1 x) (K1 y) = sim b x y

instance ( GSimilar x, GSimilar y ) => GSimilar (x :+: y) where
  gsim b (L1 x) (L1 y) = gsim b x y
  gsim b (R1 x) (R1 y) = gsim b x y
  gsim _ _ _ = False

instance ( GSimilar x, GSimilar y ) => GSimilar (x :*: y) where
  gsim b (x1 :*: y1) (x2 :*: y2) = gsim b x1 x2 && gsim b y1 y2
