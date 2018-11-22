module Lib ( 
  someFunc
) where

import System.Random
import Graphics.Gloss
import Objects
import Types
import Input
import Levels
import Utils

initialGame :: StdGen -> Game
initialGame rnd = Game
    (Player
        (Position 0 0)
        (Velocity 0 0)
        1
    )
    ( getLevel $ getRandomInt rnd 1 10 )

updateGame :: Float -> Game -> Game
updateGame dt game = gravity dt game

drawGame :: Game -> Picture
drawGame game = simple game

someFunc :: IO ()
someFunc = do
    g <- newStdGen
    play screen white 60 (initialGame g) drawGame handleGame updateGame 
    where
        screen = InWindow "Doodle Jump" (320, 480) (100, 100)
