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

initialGame :: StdGen -> GameState
initialGame rnd = Menu

updateGame :: Float -> GameState -> GameState
updateGame dt game = gravity dt game

drawGame :: GameState -> Picture
drawGame game = simple game

someFunc :: IO ()
someFunc = do
    g <- newStdGen
    play screen white 60 (initialGame g) drawGame handleGame updateGame 
    where
        screen = InWindow "Doodle Jump" (320, 480) (100, 100)
