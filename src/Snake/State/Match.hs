module Snake.State.Match
  ( MatchState(..)
  , initialMatchState
  , Snake(..)
  , SnakePart(..)
  , SnakeSlot(..)
  , Position(..)
  , moveSnake
  ) where

import Prelude hiding (Either(..))

data MatchState = MatchState
  { boardSize   :: Int
  , interations :: Int
  , snake       :: Snake
  }

type Snake = [SnakePart]
type SnakePart = (Position, Direction, SnakeSlot)

data Direction = Up | Right | Down | Left

data SnakeSlot
  =  Head0  |  Head1  |  Head2  |  Head3
  | THead0  | THead1  | THead2  | THead3
  | Tongue0 | Tongue1 | Tongue2 | Tongue3
  |  Tail0  |  Tail1  |  Tail2  |  Tail3
  |  Body0  |  Body1  |  Body2  |  Body3
  | Curve0  | Curve1  | Curve2  | Curve3
  
type Position = (Int, Int)

instance Num SnakeSlot where -- TODO: Update numeric instance if necessary
  negate = undefined
  (+)    = undefined
  (*)    = undefined
  abs    = undefined
  signum = undefined
  fromInteger n = Head0

baseBoardSize :: Int
baseBoardSize = 20

initialMatchState :: MatchState
initialMatchState = MatchState
  { boardSize = baseBoardSize
  , interations = 60
  , snake = initialSnake
  }

initialSnake :: Snake
initialSnake = [h, b, t]
  where
    sIndex = (div baseBoardSize 2) - 1
    h = ((sIndex, sIndex), Right, Head1)
    b = ((sIndex - 1, sIndex), Right, Body1)
    t = ((sIndex - 2, sIndex), Right, Tail1)

moveSnake :: Snake -> Snake
moveSnake (x:xs) = (moveSnakeHead x):(moveSnakeBody xs)

moveSnakeHead :: SnakePart -> SnakePart
moveSnakeHead ((x, y), Up, s)    = ((x, y+1), Up,    Head0)
moveSnakeHead ((x, y), Right, s) = ((x+1, y), Right, Head1)
moveSnakeHead ((x, y), Down,  s) = ((x, y-1), Down,  Head2)
moveSnakeHead ((x, y), Left,  s) = ((x+1, y), Left,  Head3)

moveSnakeBody :: [SnakePart] -> [SnakePart]
moveSnakeBody l = l

-- snakeUp :: Snake -> Snake
