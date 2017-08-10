{-# LANGUAGE DefaultSignatures #-}
module Simple.Internal.Base where

import Data.Int
import Data.Word

class Base b where
  base :: Real a => b -> a
  default base :: Real a => b -> a
  base _ = 10

instance Base Double
instance Base Float
instance Base Int
instance Base Integer
instance Base Int64
instance Base Int32
instance Base Int16
instance Base Int8
instance Base Word64
instance Base Word32
instance Base Word16
instance Base Word8

