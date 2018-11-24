module Input (
  handleGame,
  gravity
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Types
import Settings
import Levels
import Utils

handleGame :: [Int] -> Event -> GameState -> GameState
handleGame rnd (EventKey (SpecialKey KeySpace) up _ _) state =
  Game
      (Player
          (Position 0 0)
          (Velocity 0 0)
          1
          LEFT
      )
      ( getInitialLevels )
      0
      rnd
handleGame _ (EventKey (SpecialKey KeyLeft)  up _ _) state =
  _action state LEFT
handleGame _ (EventKey (SpecialKey KeyRight) up _ _) state =
  _action state RIGHT
handleGame _ _ state                  = state

_action :: GameState -> Direction -> GameState
_action Menu _   = Menu
_action (Defeat score) _ = (Defeat score)
_action (Game (Player (Position x y) (Velocity velX velY) mass dir) pl score rnd) LEFT = 
  Game (Player (Position x y) (Velocity (velX - speedConst) velY) mass LEFT) pl score rnd
_action (Game (Player (Position x y) (Velocity velX velY) mass dir) pl score rnd) RIGHT = 
  Game (Player (Position x y) (Velocity (velX + speedConst) velY) mass RIGHT) pl score rnd

gravity :: Float -> GameState -> GameState
gravity _  Menu                                   = Menu
gravity _  (Defeat score)                         = (Defeat score)
gravity dt (Game (Player pos vel mass dir) levelPattern score (rndLevel:rnd))
  | isDefeat pos vel levelPattern = (Defeat score)
  | otherwise = Game updatePlayer (updateLevelPatter (Player pos vel mass dir) levelPattern rndLevel score) (newScore pos vel) rnd
  where
    newScore (Position _ y) (Velocity _ y') 
      | y + y' > 0 = max score (score + round (y' * blockSize))
      | otherwise = score
    updatePlayer :: Player
    updatePlayer = Player (uPos pos vel) (uVel pos vel) mass dir
    uPos :: Position -> Velocity -> Position
    uPos (Position x y) (Velocity x' y') 
      | x + x' > windowWidth = Position ((-windowWidth) + x') (newY)
      | x + x' < -windowWidth = Position (windowWidth + x') (newY)
      | otherwise = Position (x + x') (newY)
       where
        newY 
          | y + y' > 0 = y
          | otherwise = y + y'
    uVel :: Position -> Velocity -> Velocity
    uVel (Position x y) (Velocity x' y')
      | isCollision (Position x (y + y')) levelPattern = Velocity x' jumpConst
      | x' >= 0 && y' >= 0 = Velocity (x' - (speedVel * dt)) (y' - (jumpVel * gravityConst * mass * dt))
      | x' <= 0 && y' >= 0 = Velocity (x' + (speedVel * dt)) (y' - (jumpVel * gravityConst * mass * dt))
      | y' < 0             = Velocity x' (y' - (jumpVel * gravityConst * mass * dt))
      | otherwise          = Velocity x' y'

isCollision :: Position -> LevelPattern -> Bool
isCollision playerPos (LevelPattern platforms ) = or checkPl
  where
    checkPl = map (\pl -> checkCollision playerPos pl) platforms


isDefeat :: Position -> Velocity -> LevelPattern -> Bool
isDefeat
  (Position x y)
  (Velocity x' y')
  (LevelPattern platforms)
  | y + y' < -windowHeight = True
  | or checkMn = True
  | otherwise = False
    where
       checkMn = map (\mn -> checkCollision (Position x y) mn) (filter (\pl -> isMonster pl) platforms)
       isMonster :: Platform -> Bool
       isMonster (Platform _ _ _ t _) = t == MONSTER1 || t == MONSTER2
