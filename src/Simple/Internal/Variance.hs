module Simple.Internal.Variance where

data Variance
  = Variance
    { count :: {-# UNPACK #-} !Double
    , mean  :: {-# UNPACK #-} !Double
    , mean2 :: {-# UNPACK #-} !Double
    , minimum_ :: {-# UNPACK #-} !Double
    , maximum_ :: {-# UNPACK #-} !Double
    } deriving (Generic,ToJSON,FromJSON)


updateVariance :: Micros -> Variance -> Variance
updateVariance (Micros mt) Variance {..} =
  let !mtd = fromInteger mt
      !count' = count + 1
      !delta = mtd - mean
      !mean' = mean + (delta / count')
      !mean2' = mean2 + delta * (mtd - mean')
      !mx = max mtd maximum_
      !mn = if minimum_ == 0 then mtd else min mtd minimum_
  in Variance count' mean' mean2' mn mx

variance :: Variance -> Maybe Double
variance Variance {..} = if count < 2 then Nothing else Just $ mean2 / (count - 1)

stdDev :: Variance -> Maybe Double
stdDev = fmap sqrt . variance

class HasVariance a where
  emptyVarianceArray :: Array Variance
  updateVariance :: a -> Array Variance -> Array Variance

  
