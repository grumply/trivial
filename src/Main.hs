module Main where

import Trivial

main = do
  run $ forM_ [1..10] $ \i -> do
    (_,r) <- bench (show i) $
      return (foldr (+) 0 [1..i :: Int])
    expect (r > 0)
