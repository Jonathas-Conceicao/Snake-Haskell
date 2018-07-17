module Snake.State.Match
  ( MatchState(..)
  , initialMatchState
  , Snake(..)
  , SnakePart(..)
  , SnakeSlot(..)
  , Position(..)
  , Direction(..)
  , Food(..)
  , FoodSlot(..)
  , moveSnake
  ) where

import Prelude hiding (Either(..))
import System.Random

data MatchState = MatchState
  { boardSize   :: Int
  , foodList    :: [Food]
  , interations :: Int
  , snake       :: Snake
  , eaten       :: Bool
  , food        :: Food
  }

type Snake = [SnakePart]
type Food  = (Position, FoodSlot)
type SnakePart = (Position, Direction, SnakeSlot)

data Direction = Up | Right | Down | Left
  deriving (Eq, Ord, Show, Enum)

data FoodSlot = Apple
data SnakeSlot
  =  Head0  |  Head1  |  Head2  |  Head3
  | THead0  | THead1  | THead2  | THead3
  | Tongue0 | Tongue1 | Tongue2 | Tongue3
  |  Tail0  |  Tail1  |  Tail2  |  Tail3
  |  Body0  |  Body1  |  Body2  |  Body3
  | Curve0  | Curve1  | Curve2  | Curve3
  deriving (Eq, Ord, Show, Enum)
  
type Position = (Int, Int)

-- instance Num Direction where
--   negate = id
--   (+) x Up = succ x
--   (+) x y = x + (pred y)
--   (*)    = undefined
--   abs    = undefined
--   signum Up = 0
--   signum _ = 1
--   fromInteger 0 = Up
--   fromInteger x = succ $ fromInteger $ x - 1

-- instance Num SnakeSlot where
--   negate = id
--   (+) x Head0 = succ x
--   (+) x y = x + (pred y)
--   (*)    = undefined
--   abs    = undefined
--   signum Head0 = 0
--   signum _ = 1
--   fromInteger 0 = Head0
--   fromInteger x = succ $ fromInteger $ x - 1

baseBoardSize :: Int
baseBoardSize = 20

initialMatchState :: RandomGen g => g -> MatchState
initialMatchState g = MatchState
  { boardSize = baseBoardSize
  , foodList = fl
  , interations = 60
  , snake = initialSnake
  , eaten = False
  , food  = newFood fl
  }
  where
    fl = newFoodList g

newFoodList :: RandomGen g => g -> [Food]
newFoodList g = ((x, y), Apple):(newFoodList g'')
  where
    (x, g')  = randomR range g
    (y, g'') = randomR range g'
    range = (0, baseBoardSize -2)

newFood :: [Food] -> Food
newFood = head

initialSnake :: Snake
initialSnake = [h, b, t]
  where
    sIndex = (div baseBoardSize 2) - 1
    h = ((sIndex, sIndex), Down, Head1)
    b = ((sIndex - 1, sIndex), Right, Body1)
    t = ((sIndex - 2, sIndex), Right, Tail1)

moveSnake :: Bool -> Snake -> Snake
moveSnake e (x:xs) = (moveSnakeHead x) ++ (moveSnakeBody e xs)

moveSnakeHead :: SnakePart -> [SnakePart]
moveSnakeHead ((x, y), Up, s)    = ((x, y+1), Up,    Head0):[genNeck (x,y) s Up]
moveSnakeHead ((x, y), Right, s) = ((x+1, y), Right, Head1):[genNeck (x,y) s Right]
moveSnakeHead ((x, y), Down,  s) = ((x, y-1), Down,  Head2):[genNeck (x,y) s Down]
moveSnakeHead ((x, y), Left,  s) = ((x-1, y), Left,  Head3):[genNeck (x,y) s Left]

moveSnakeBody :: Bool -> [SnakePart] -> [SnakePart]
moveSnakeBody True l = l
moveSnakeBody _ ((p, Up,    _):(_, _, _):[]) = [(p, Up,    Tail0)]
moveSnakeBody _ ((p, Right, _):(_, _, _):[]) = [(p, Right, Tail1)]
moveSnakeBody _ ((p, Down,  _):(_, _, _):[]) = [(p, Down,  Tail2)]
moveSnakeBody _ ((p, Left,  _):(_, _, _):[]) = [(p, Left,  Tail3)]
moveSnakeBody b (x:xs) = x:(moveSnakeBody b xs)

genNeck :: Position -> SnakeSlot -> Direction -> SnakePart
genNeck p Head1 Up = (p, Up, Curve1)
genNeck p Head3 Up = (p, Up, Curve0)
genNeck p _ Up = (p, Up, Body0)

genNeck p Head0 Right = (p, Right, Curve2)
genNeck p Head2 Right = (p, Right, Curve0)
genNeck p _ Right = (p, Right, Body1)

genNeck p Head1 Down = (p, Down, Curve3)
genNeck p Head3 Down = (p, Down, Curve2)
genNeck p _ Down = (p, Down, Body2)

genNeck p Head0 Left = (p, Left, Curve3)
genNeck p Head2 Left = (p, Left, Curve1)
genNeck p _ Left = (p, Left, Body3)
