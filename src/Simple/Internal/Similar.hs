{-# LANGUAGE DefaultSignatures #-}
module Simple.Internal.Similar where

import Data.Int
import Data.Word

class Similar a where
  similar :: Real b => b -> a -> a -> Bool

  {-# INLINE dissimilar #-}
  dissimilar :: Real b => b -> a -> a -> Bool
  dissimilar b x y = not (similar b x y)

  default similar :: (Real b, Real a) => b -> a -> a -> Bool
  -- | Check if two reals are of similar magnitude given a scaling base `b`.
  --
  -- In general, as x grows, relative epsilon shrinks as a factor of log_b x.
  --
  -- During development, `e` is a good base for approximation of results.
  --
  -- Note: For b < 1, similar b _ _ -> False
  --
  {-# INLINE similar #-}
  -- similar :: (Real a,Real b) => a -> b -> b -> Bool
  similar _ 0 0 = True
  similar (realToFrac -> b) (realToFrac -> x) (realToFrac -> y) =
      -- As b ↘ 1, epsilon ↘ 0
      let epsilon = (x :: Double) / (logBase b (x + 1))
          (lo,hi) = (x - epsilon,x + epsilon)
      in lo <= y && y <= hi


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

