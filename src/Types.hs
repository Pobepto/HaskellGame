module Types (
  Game (Game),
  Position (Position),
  Player (Player),
  Direction (LEFT, RIGHT),
) where

data Direction = LEFT | RIGHT deriving Eq

data Game = Game
  {
    player :: Player
  }

data Position = Position
  {
    x :: Double,
    y :: Double
  }

data Player = Player
  {
    playerPosition :: Position,
    playerMass     :: Double
  }