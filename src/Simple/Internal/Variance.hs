{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
module Simple.Internal.Variance (Vary(..),Varied,Variance(..),stdDev,variance,varies,getVariance) where

import Data.HashMap.Lazy as HM

import Data.Int
import Data.Word

import Data.Text as T
import Data.Aeson
import Data.Maybe
import GHC.Generics as G
import GHC.TypeLits

import qualified Data.Vector as V

-- Generic implementation of variance analysis. Simply:
--
-- > instance Vary <SomeType>

data Variance
  = Variance
    { count :: {-# UNPACK #-} !Double
    , mean  :: {-# UNPACK #-} !Double
    , mean2 :: {-# UNPACK #-} !Double
    , minimum_ :: {-# UNPACK #-} !Double
    , maximum_ :: {-# UNPACK #-} !Double
    } deriving (Eq,Ord,Generic,ToJSON,FromJSON,Show)

newtype Varied = Varied (HashMap String Variance)
 deriving (Show,Eq)

{-# INLINE addToVariance #-}
addToVariance :: Real a => a -> Variance -> Variance
addToVariance (realToFrac -> a) Variance {..} =
  let !count' = count + 1
      !delta = a - mean
      !mean' = mean + (delta / count')
      !mean2' = mean2 + delta * (a - mean')
      !mx = max a maximum_
      !mn = if minimum_ == 0 then a else min a minimum_
  in Variance count' mean' mean2' mn mx

{-# INLINE variance #-}
variance :: Variance -> Maybe Double
variance Variance {..} = if count < 2 then Nothing else Just $ mean2 / (count - 1)

{-# INLINE stdDev #-}
stdDev :: Variance -> Maybe Double
stdDev = fmap sqrt . variance

{-# INLINE varies #-}
varies :: (Foldable f, Vary a) => f a -> Varied
varies = Prelude.foldr (varied "") (Varied mempty)

{-# INLINE getVariance #-}
getVariance :: String -> Varied -> Maybe Variance
getVariance s (Varied v) = HM.lookup s v

class Vary a where
  varied :: String -> a -> Varied -> Varied
  default varied :: (Generic a, GVary (Rep a)) => String -> a -> Varied -> Varied
  varied _ a hm = gUpdateVariance "" (from a) hm

{-# INLINE gUpdateRealVariance #-}
gUpdateRealVariance :: (Real a) => String -> a -> Varied -> Varied
gUpdateRealVariance nm a (Varied hm) =
  Varied (HM.alter (Just . maybe (addToVariance a (Variance 0 0 0 0 0)) (addToVariance a)) nm hm)

instance {-# OVERLAPPABLE #-} Vary a where
  varied _ _ = id

instance {-# OVERLAPPING #-} Vary Double where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Float where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Int where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Integer where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Int8 where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Int16 where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Int32 where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Int64 where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Word where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Word8 where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Word16 where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Word32 where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Word64 where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} (Real a, Vary a) => Vary (HashMap String a) where
  varied nm a hm = varied nm (HM.toList a) hm

instance {-# OVERLAPPING #-} (Real a, Foldable f, Vary a) => Vary (f (String,a)) where
  varied nm a (Varied hm) = Varied $
    Prelude.foldr
      (\(k,v) m ->
        let n = (nm ++ "." ++ k)
        in HM.alter (Just . maybe (addToVariance v (Variance 0 0 0 0 0)) (addToVariance v)) n m
      )
      hm
      a

instance {-# OVERLAPPING #-} (Real a, Vary a) => Vary (HashMap Text a) where
  varied nm a hm = varied nm (HM.toList a) hm

instance {-# OVERLAPPING #-} (Real a, Foldable f, Vary a) => Vary (f (Text,a)) where
  varied nm a (Varied hm) = Varied $ 
    Prelude.foldr
      (\(k,v) m ->
        let n = (nm ++ "." ++ T.unpack k)
        in HM.alter (Just . maybe (addToVariance v (Variance 0 0 0 0 0)) (addToVariance v)) n m
      )
      hm
      a

instance {-# OVERLAPPING #-} (Real a, Vary a) => Vary (V.Vector a) where
  varied nm a (Varied hm) = Varied $
    V.ifoldr' (\i v m ->
       let n = nm ++ "." ++ show i
       in HM.alter (Just . maybe (addToVariance v (Variance 0 0 0 0 0)) (addToVariance v)) n m
     )
     hm
     a

class GVary a where
  gUpdateVariance :: String -> a x -> Varied -> Varied

instance ( Datatype d
         , GVary a
         ) => GVary (D1 d a) where
  gUpdateVariance base m@(M1 d) hm =
    let dn = datatypeName m
    in gUpdateVariance (if Prelude.null base then dn else base ++ "." ++ dn) d hm

instance ( Constructor c
         , GVary a
         ) => GVary (C1 c a) where
  gUpdateVariance base m@(M1 c) hm =
    let cn = conName m
    in gUpdateVariance (base ++ "." ++ cn) c hm

instance {-# OVERLAPPING #-}
         ( Selector ('MetaSel 'Nothing u s l)
         , GUnlabeledFieldVary a
         ) => GVary (S1 ('MetaSel 'Nothing u s l) a) where
  gUpdateVariance base m@(M1 s) hm =
    gUpdateUnlabeledFieldVariance base 1 s hm

instance {-# OVERLAPPABLE #-}
         ( Selector s
         , GVary a
         ) => GVary (S1 s a)  where
  gUpdateVariance base m@(M1 s) hm =
    let sn = selName m
    in gUpdateVariance (base ++ "." ++ sn) s hm

instance (Vary a) => GVary (K1 r a) where
  gUpdateVariance base (K1 a) hm =
    varied base a hm

instance (GVary a, GVary b) => GVary (a :+: b) where
  gUpdateVariance base (L1 a) = gUpdateVariance base a
  gUpdateVariance base (R1 b) = gUpdateVariance base b

instance {-# OVERLAPPING #-}
         ( Selector ('MetaSel 'Nothing u s l)
         , GRecordVary (S1 ('MetaSel 'Nothing u s l) a :*: sb)
         ) => GVary (S1 ('MetaSel 'Nothing u s l) a :*: sb) where
  gUpdateVariance base ss hm = gUpdateRecordVariance base 0 ss hm

instance {-# OVERLAPPABLE #-}
         ( GVary a
         , GVary b
         ) => GVary (a :*: b) where
  gUpdateVariance base (a :*: b) hm = gUpdateVariance base b (gUpdateVariance base a hm)

class GUnlabeledFieldVary a where
  gUpdateUnlabeledFieldVariance :: String -> Int -> a x -> Varied -> Varied

instance (Selector s, GVary a) => GUnlabeledFieldVary (S1 ('MetaSel 'Nothing u s l) a) where
  gUpdateUnlabeledFieldVariance base index m@(M1 s) hm =
    let sn = show index
    in gUpdateVariance (base ++ "." ++ sn) s hm

instance (GVary (S1 s a)) => GUnlabeledFieldVary (S1 s a) where
  gUpdateUnlabeledFieldVariance base _ = gUpdateVariance base

instance (Vary a) => GUnlabeledFieldVary (K1 r a) where
  gUpdateUnlabeledFieldVariance base index (K1 a) hm =
    varied (base ++ "." ++ show index) a hm

class GRecordVary a where
  gUpdateRecordVariance :: String -> Int -> a x -> Varied -> Varied

instance {-# OVERLAPPABLE #-}
         ( GUnlabeledFieldVary s
         ) => GRecordVary s where
  gUpdateRecordVariance base index s hm = gUpdateUnlabeledFieldVariance base index s hm

instance {-# OVERLAPPING #-}
         ( Selector sa
         , GVary a
         , GRecordVary sb
         ) => GRecordVary (S1 sa a :*: sb) where
   gUpdateRecordVariance base index (sa :*: sb) hm =
     let nm = base ++ "." ++ show index
     in gUpdateRecordVariance base (index + 1) sb (gUpdateVariance nm sa hm)
