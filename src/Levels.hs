module Levels (
  getLevel
) where

import System.Random
import Types


level_1 :: LevelPattern
level_1 = LevelPattern [
      (Platform (Position 0 0) 5 1),
      (Platform (Position 4 4) 5 1),
      (Platform (Position (-4) (-4)) 5 1),
      (Platform (Position (-8) (8)) 5 1),
      (Platform (Position 8 (-4)) 5 1)
    ]

level_2 = LevelPattern [
      (Platform (Position 0 (-4)) 5 1),
      (Platform (Position 0 0) 5 1),
      (Platform (Position 0 4) 5 1)
    ]

getLevel :: Int -> LevelPattern
getLevel level_id
  | level_id <= 5 = level_1
  | otherwise = level_2
