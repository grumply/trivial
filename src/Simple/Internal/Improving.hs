{-# LANGUAGE DefaultSignatures #-}
module Simple.Internal.Improving where

class Improving a where
  improving :: a -> a -> Bool
  improvingShow :: a -> String

  default improving :: (Real a) => a -> a -> Bool
  improving a b = a < b -- monotonically increasing as default

  default improvingShow :: (Real a) => a -> String
  improvingShow _ = "<"

