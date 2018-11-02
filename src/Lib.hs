module Lib ( 
  someFunc
) where

import CodeWorld
import Objects
import Types
import Input

initialGame :: Game
initialGame = Game
    (Player
        (Position (-4) 2)
        (Velocity 0 0)
        1
    )
    ([
        (Platform (Position 0 0) 4 1),
        (Platform (Position 4 4) 4 1),
        (Platform (Position (-4) (-4)) 4 1),
        (Platform (Position (-8) (8)) 4 1),
        (Platform (Position (8) (-4)) 4 1)
    ])

updateGame :: Double -> Game -> Game
updateGame dt game = gravity dt game

drawGame :: Game -> Picture
drawGame game = simple game

someFunc :: IO ()
someFunc = interactionOf initialGame updateGame handleGame drawGame
