module Main where

import Trivial

main = do
  run $ forM_ [1..10] $ \i -> do
    let test = return (foldr (+) 0 [1..i :: Int])
    bench (show i) test $ \(_,sum) ->
      expect (sum > 0)
