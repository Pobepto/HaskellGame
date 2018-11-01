module Lib ( 
  someFunc
) where

import CodeWorld
import Objects
import Types
import Input

initialGame :: Game
initialGame = Game (Player (Position 0 1) 1)

updateGame :: Double -> Game -> Game
updateGame dt game = gravity dt game

drawGame :: Game -> Picture
drawGame game = simple game

someFunc :: IO ()
someFunc = interactionOf initialGame updateGame handleGame drawGame
