{-# language ViewPatterns #-}
module Simple.Internal.Utilities where

-- for debugging of mag and similar instead
-- of just returning bounds for them. This should
-- be fast enough that it doesn't matter.


-- Note that mag cannot be minimized!
minimize :: Integral a => (a -> Bool) -> a -> a
minimize f (abs -> initial) = go lo hi
  where

    (lo,hi) = bounds initial
      where
        bounds 0 = (0,0)
        bounds a
          | f a = bounds (a `div` 2)
          | otherwise = (a,a * 2)

    go lo hi =
      let mid = (lo + hi) `div` 2 in
      if hi - mid <= 1 then
        mid
      else if f mid then
        go lo mid
      else
        go mid hi

maximize :: Integral a => (a -> Bool) -> a -> a
maximize f (abs -> initial) = go lo hi
  where

    (lo,hi) = bounds initial
      where
        bounds 0 = bounds 1
        bounds a
          | f a = bounds (a * 2)
          | otherwise = (a `div` 2,a)

    go lo hi =
      let mid = (lo + hi) `div` 2 in
      if mid - lo <= 1 then
        mid
      else if f mid then
        go mid hi
      else
        go lo mid

bound :: Integral a => (a -> Bool) -> a -> (a,a)
bound f initial = (minimize f initial,maximize f initial)
