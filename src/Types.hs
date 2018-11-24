module Types (
  GameState (Game, Menu, Defeat),
  Position (Position),
  Velocity (Velocity),
  Acceleration (Acceleration),
  Player (Player),
  Platform (Platform),
  PlatformType (GREEN, BLUE, WHITE, MONSTER1, MONSTER2),
  Direction (LEFT, RIGHT),
  LevelPattern (LevelPattern),
) where

data Direction = LEFT | RIGHT deriving Eq

data GameState = Game {
  player :: Player,
  level  :: LevelPattern,
  score  :: Int,
  rnd    :: [Int]
} | Menu 
  | Defeat {
  score :: Int
}

data Position = Position
  {
    x :: Float,
    y :: Float
  }

data Velocity = Velocity
  {
    velX :: Float,
    velY :: Float
  }

data Acceleration = Acceleration
  {
    accX :: Float,
    accY :: Float
  }
  
data LevelPattern = LevelPattern
  {
    platforms :: [Platform]
  }

data Player = Player
  {
    playerPosition     :: Position,
    playerVelocity     :: Velocity,
    playerMass         :: Float,
    playerDirection    :: Direction
  }

data PlatformType = GREEN | BLUE | WHITE | MONSTER1 | MONSTER2 deriving Eq

data Platform = Platform
  {
    platformPosition  :: Position,
    platformWidth     :: Float,
    platformHeight    :: Float,
    platformType      :: PlatformType,
    platformDirection :: Direction
  }

