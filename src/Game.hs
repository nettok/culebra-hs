module Game
  ( Pos (..)
  , Dir (..)
  , Color (..)
  , Snake (..)
  , Bounds (..)
  , GameState (..)
  , inverse
  , go
  , canGo
  , move
  , canMove
  , snakeToBodyPositions
  ) where

import Prelude hiding (Left, Right)

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

data Bounds = Bounds
  { lowerBounds :: Pos
  , upperBounds :: Pos
  } deriving (Eq, Show)

data GameState = GameState
  { gsSnakes :: [Snake]
  } deriving (Eq, Show)

inverse :: Dir -> Dir
inverse Up    = Down
inverse Down  = Up
inverse Left  = Right
inverse Right = Left

go :: Pos -> Dir -> Pos
go pos Up    = pos { posY = posY pos - 1 }
go pos Down  = pos { posY = posY pos + 1 }
go pos Left  = pos { posX = posX pos - 1 }
go pos Right = pos { posX = posX pos + 1 }

canGo :: Bounds -> Pos -> Dir -> Bool
canGo bounds pos Up    = posY pos > (posY . lowerBounds) bounds
canGo bounds pos Down  = posY pos < (posY . upperBounds) bounds
canGo bounds pos Left  = posX pos > (posX . lowerBounds) bounds
canGo bounds pos Right = posX pos < (posX . upperBounds) bounds

move :: Snake -> Dir -> Snake
move snake dir = snake
  { snakeHead  = go (snakeHead snake) dir
  , snakeMoves = dir : init (snakeMoves snake)
  }

canMove :: Bounds -> Snake -> Dir -> Snake -> Bool
canMove = undefined

snakeToBodyPositions :: Snake -> [Pos]
snakeToBodyPositions snake = snakeToBodyPositions' (snakeHead snake) (snakeMoves snake)

snakeToBodyPositions' :: Pos -> Moves -> [Pos]
snakeToBodyPositions' _ [] = []
snakeToBodyPositions' currPos (mov:movs) = currPos : snakeToBodyPositions' (go currPos $ inverse mov) movs
