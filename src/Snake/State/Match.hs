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

data Direction = Up | Down | Left | Right

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

initialMatchState :: MatchState
initialMatchState = MatchState
  { boardSize = 20
  , interations = 60
  , snake = initialSnake
  }

initialSnake :: Snake
initialSnake = [h, b, t]
  where
    sIndex = 10
    h = ((sIndex, sIndex), Right, Head1)
    b = ((sIndex - 1, sIndex), Right, Body1)
    t = ((sIndex - 2, sIndex), Right, Tail1)

moveSnake :: Snake -> Snake
moveSnake l = l

-- snakeUp :: Snake -> Snake
