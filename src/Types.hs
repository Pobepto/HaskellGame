module Types (
  Game (Game),
  Position (Position),
  Velocity (Velocity),
  Acceleration (Acceleration),
  Player (Player),
  Platform (Platform),
  Direction (LEFT, RIGHT),
  LevelPattern (LevelPattern)
) where

data Direction = LEFT | RIGHT deriving Eq

data Game = Game
  {
    player :: Player,
    level  :: LevelPattern
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
    playerMass         :: Float
  }

data Platform = Platform
  {
    platformPosition :: Position,
    platformWidth    :: Float,
    platformHeight   :: Float
  }
