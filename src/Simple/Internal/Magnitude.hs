{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
module Simple.Internal.Magnitude where

import Data.Int
import Data.Word

class Magnitude a where
  mag :: (Real b) => b -> a -> a -> Bool

  default mag :: (Real b, Real a) => b -> a -> a -> Bool
  mag (realToFrac -> b) (realToFrac -> x) a@(realToFrac -> y)=
    let o = realToFrac $ floor (logBase b (x :: Double)) + 1
        hi :: Double
        hi = b ** o
        e = x / (logBase b o + 1)
    in y <= hi + e

magnitude :: Magnitude a => a -> a -> Bool
magnitude = mag (exp 1)

instance Magnitude Double
instance Magnitude Float
instance Magnitude Int
instance Magnitude Integer
instance Magnitude Int64
instance Magnitude Int32
instance Magnitude Int16
instance Magnitude Int8
instance Magnitude Word64
instance Magnitude Word32
instance Magnitude Word16
instance Magnitude Word8

