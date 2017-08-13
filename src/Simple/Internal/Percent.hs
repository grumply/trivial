{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Simple.Internal.Percent where

import Simple.Internal.Pretty
import Simple.Internal.Variance

import GHC.Generics

import Data.Aeson

import Text.Printf

newtype Percent = Percent { getPercent :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,ToJSON,FromJSON)

instance Vary Percent

instance Pretty Percent where
    pretty (Percent r)
      | isNaN r   = "0%"
      | otherwise = printf "%.2f%%" (100 * r)

mkPercent :: (Real a, Real b) => a -> b -> Percent
mkPercent a b =
  let b' = realToFrac b
  in if b' == 0 then
       Percent 0
     else
       Percent $ realToFrac a / b'
