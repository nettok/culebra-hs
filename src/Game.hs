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
  -- demo
  , start
  , advanceRandom
  ) where

import Prelude hiding (Left, Right)
import System.Random (randomRIO)

data Pos = Pos
  { posX :: Int
  , posY :: Int
  } deriving (Eq, Show)

data Dir = Up | Down | Left | Right deriving (Eq, Show, Enum)

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

canMove :: Bounds -> Snake -> Dir -> Bool
canMove bounds snake dir = canGo bounds (snakeHead snake) dir -- && not (collided (snakeHead snake) (tail $ snakeToBodyPositions snake))

collided :: Pos -> [Pos] -> Bool
collided pos poss = pos `elem` poss

snakeToBodyPositions :: Snake -> [Pos]
snakeToBodyPositions snake = init $ scanl (\pos mov -> go pos $ inverse mov) (snakeHead snake) (snakeMoves snake)

-- demo

start :: GameState
start = GameState
  { gsSnakes =
    [ Snake { snakeHead = Pos { posX = 500, posY = 500 }, snakeMoves = replicate 6 Left, snakeColor = Color 1.0 0.0 1.0 1.0 } ]
  }

advanceRandom :: GameState -> IO GameState
advanceRandom gs = do
  moved <- traverse moveRandom (gsSnakes gs)
  return $ gs { gsSnakes = moved }

randomDir :: IO Dir
randomDir = do
  n <- randomRIO (0, 3)
  return $ [Up, Down, Left, Right] !! n

moveRandom :: Snake -> IO Snake
moveRandom snake = do
  dir <- randomDir
  if canMove gameBounds snake dir
    then return $ move snake dir
    else moveRandom snake

gameBounds :: Bounds
gameBounds = Bounds
  { lowerBounds = Pos { posX = 0,    posY = 0 }
  , upperBounds = Pos { posX = 1000, posY = 1000 }
  }
