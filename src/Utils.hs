module Utils (
  getRandomInt
) where

import System.Random

getRandomInt :: StdGen -> Int -> Int -> Int
getRandomInt rnd min max = fst $ randomR (min, max) rnd