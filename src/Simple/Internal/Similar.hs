{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
module Simple.Internal.Similar where

import Data.Int
import Data.Word

class Similar a where
  sim :: (Real b) => b -> a -> a -> Bool
  default sim :: (Eq a, Real a, Real b) => b -> a -> a -> Bool
  sim _ 0 0 = True
  sim (realToFrac -> b) (realToFrac -> x) (realToFrac -> y) =
      -- As b ↘ 1, epsilon ↘ 0
      let epsilon = (x :: Double) / (logBase b (x + 1) + 1)
          (lo,hi) = (x - epsilon,x + epsilon)
      in lo <= y && y <= hi

similar :: (Similar a) => a -> a -> Bool
similar = sim (exp 1)

instance Similar Double
instance Similar Float
instance Similar Int
instance Similar Integer
instance Similar Int64
instance Similar Int32
instance Similar Int16
instance Similar Int8
instance Similar Word64
instance Similar Word32
instance Similar Word16
instance Similar Word8

