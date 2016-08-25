module Game
  ( Pos (..)
  , Dir (..)
  , Color (..)
  , Snake (..)
  , GameState (..)
  ) where

data Pos = Pos
  { posX :: Int
  , posY :: Int
  } deriving (Eq, Show)

data Dir = Up | Down | Left | Right deriving (Eq, Show)

data Color = Color Float Float Float Float deriving (Eq, Show) -- R G B Alpha [0.0, 1.0]

type Moves = [Dir]

data Snake = Snake
  { snakeHead  :: Pos
  , snakeMoves :: Moves
  , snakeColor :: Color
  } deriving (Eq, Show)

data GameState = GameState
  { gsSnakes :: [Snake]
  } deriving (Eq, Show)
