module Lib ( 
  someFunc
) where

import System.Random

import CodeWorld
import Objects
import Types
import Input
import Levels
import Utils

initialGame :: StdGen -> Game
initialGame rnd = Game
    (Player
        (Position (-4) 2)
        (Velocity 0 0)
        1
    )
    ( getLevel $ getRandomInt rnd 1 10 )

updateGame :: Double -> Game -> Game
updateGame dt game = gravity dt game

drawGame :: Game -> Picture
drawGame game = simple game

someFunc :: IO ()
someFunc = do
    g <- newStdGen
    interactionOf (initialGame g) updateGame handleGame drawGame
