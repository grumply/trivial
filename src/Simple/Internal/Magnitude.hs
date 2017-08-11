{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
module Simple.Internal.Magnitude where

import Data.Int
import Data.Word

import Simple.Internal.Base

class Ord a => Magnitude a where
  mag :: (Real b) => b -> a -> a -> Bool
  default mag :: (Base a, Real a) => b -> a -> a -> Bool
  mag _ (realToFrac -> x) (realToFrac -> y)=
    let o = realToFrac (floor (logBase (base y) (x :: Double)))
        hi :: Double
        hi = (realToFrac $ base y) ** (o + 1)
    in y < hi

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

