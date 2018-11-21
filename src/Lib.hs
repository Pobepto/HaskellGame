module Lib ( 
  someFunc
) where

import System.Random

import CodeWorld
import Objects
import Types
import Input
import Levels

initialGame :: StdGen -> Game
initialGame rnd = Game
    (Player
        (Position (-4) 2)
        (Velocity 0 0)
        1
    )
    ( getLevel getInt )
    where
        getInt :: Int
        getInt = fst $ randomR (1, 10) rnd

updateGame :: Double -> Game -> Game
updateGame dt game = gravity dt game

drawGame :: Game -> Picture
drawGame game = simple game

someFunc :: IO ()
someFunc = do
    g <- newStdGen
    interactionOf (initialGame g) updateGame handleGame drawGame
