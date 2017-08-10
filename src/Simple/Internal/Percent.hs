{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Simple.Internal.Percent where

import Simple.Internal.Pretty as Export

import GHC.Generics

import Data.Aeson

import Text.Printf

newtype Percent = Percent { getPercent :: Double }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,ToJSON,FromJSON)

instance Pretty Percent where
    pretty (Percent r)
      | isNaN r   = "0%"
      | otherwise = printf "%.2f%%" (100 * r)

mkPercent :: (Real a, Real b) => a -> b -> Percent
mkPercent a b = Percent $ realToFrac a / realToFrac b
