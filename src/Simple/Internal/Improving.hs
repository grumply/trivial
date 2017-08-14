{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Simple.Internal.Improving where

import Data.Int
import Data.Word

import GHC.Generics

class Improving a where
  improving :: a -> a -> Bool
  improvingShow :: a -> String

  default improving :: (Generic a,GImproving (Rep a)) => a -> a -> Bool
  improving a b = gimproving (from a) (from b)

  default improvingShow :: (Generic a,GImproving (Rep a)) => a -> String
  improvingShow a = gimprovingShow (from a)

improvingReals :: (Real a) => a -> a -> Bool
improvingReals = (<)

improvingRealsShow :: (Real a) => a -> String
improvingRealsShow _ = "<"

instance Improving Double where
  improving = improvingReals
  improvingShow = improvingRealsShow
instance Improving Float where
  improving = improvingReals
  improvingShow = improvingRealsShow
instance Improving Int where
  improving = improvingReals
  improvingShow = improvingRealsShow
instance Improving Integer where
  improving = improvingReals
  improvingShow = improvingRealsShow
instance Improving Int64 where
  improving = improvingReals
  improvingShow = improvingRealsShow
instance Improving Int32 where
  improving = improvingReals
  improvingShow = improvingRealsShow
instance Improving Int16 where
  improving = improvingReals
  improvingShow = improvingRealsShow
instance Improving Int8 where
  improving = improvingReals
  improvingShow = improvingRealsShow
instance Improving Word64 where
  improving = improvingReals
  improvingShow = improvingRealsShow
instance Improving Word32 where
  improving = improvingReals
  improvingShow = improvingRealsShow
instance Improving Word16 where
  improving = improvingReals
  improvingShow = improvingRealsShow
instance Improving Word8 where
  improving = improvingReals
  improvingShow = improvingRealsShow

class GImproving a where
  gimproving :: a x -> a x -> Bool
  gimprovingShow :: a x -> String

instance ( Datatype d, GImproving a ) => GImproving ( D1 d a ) where
  gimproving (M1 d1) (M1 d2) = gimproving d1 d2
  gimprovingShow (M1 d1) = gimprovingShow d1

instance ( Constructor c, GImproving a ) => GImproving ( C1 c a ) where
  gimproving (M1 c1) (M1 c2) = gimproving c1 c2
  gimprovingShow (M1 c1) = gimprovingShow c1

instance ( Selector s, GImproving a ) => GImproving ( S1 s a ) where
  gimproving (M1 s1) (M1 s2) = gimproving s1 s2
  gimprovingShow (M1 s1) = gimprovingShow s1

instance ( Improving a ) => GImproving ( K1 r a ) where
  gimproving (K1 x) (K1 y) = improving x y
  gimprovingShow (K1 x) = improvingShow x

instance ( GImproving x, GImproving y ) => GImproving (x :+: y) where
  gimproving (L1 x) (L1 y) = gimproving x y
  gimproving (R1 x) (R1 y) = gimproving x y
  gimproving _ _ = False
  gimprovingShow (L1 x) = gimprovingShow x
  gimprovingShow (R1 y) = gimprovingShow y

instance ( GImproving x, GImproving y ) => GImproving (x :*: y) where
  gimproving (x1 :*: y1) (x2 :*: y2) = gimproving x1 x2 && gimproving y1 y2
  gimprovingShow (x1 :*: y1) =
    let is1 = gimprovingShow x1
        is2 = gimprovingShow y1
    in if is1 /= is2 then "~" else is1

