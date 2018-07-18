module Snake.State
  ( GameState(..)
  , initialGameState
  , resetGameState
  , Snake(..)
  , SnakePart(..)
  , SnakeSlot(..)
  , Position(..)
  , Direction(..)
  , Food(..)
  , FoodSlot(..)
  , nextFood
  , moveSnake
  , checkDefeat
  , checkEaten
  ) where

import Prelude hiding (Either(..))
import System.Random

data GameState = GameState
  { boardSize   :: Int
  , foodList    :: [Food]
  , interations :: Int
  , speed       :: Int
  , score       :: Int
  , defeated    :: Bool
  , snake       :: Snake
  , eaten       :: Bool
  , food        :: Food
  }

type Snake = [SnakePart]
type Food  = (Position, FoodSlot)
type SnakePart = (Position, Direction, SnakeSlot)

data Direction = Up | Right | Down | Left
  deriving (Eq, Ord, Show, Enum)

data FoodSlot = Apple | None
data SnakeSlot
  =  Head0  |  Head1  |  Head2  |  Head3
  | THead0  | THead1  | THead2  | THead3
  | Tongue0 | Tongue1 | Tongue2 | Tongue3
  |  Tail0  |  Tail1  |  Tail2  |  Tail3
  |  Body0  |  Body1  |  Body2  |  Body3
  | Curve0  | Curve1  | Curve2  | Curve3
  deriving (Eq, Ord, Show, Enum)
  
type Position = (Int, Int)

baseBoardSize :: Int
baseBoardSize = 18

newGen :: GameState -> StdGen
newGen = mkStdGen . (uncurry (-)) . fst . food

resetGameState :: GameState -> GameState
resetGameState s = s
  { defeated = False
  , interations = 0
  , score = 0
  , snake = is
  , foodList = fl
  , food = f
  } 
  where
    is = initialSnake
    (f, fl) = nextFood (newFoodList (newGen s)) is
  
initialGameState :: RandomGen g => g -> GameState
initialGameState g = GameState
  { boardSize = baseBoardSize
  , foodList = newFoodList g
  , interations = 0
  , speed = 2
  , score = 0
  , defeated = True
  , snake = []
  , eaten = False
  , food  = ((0, 0), None)
  }

newFoodList :: RandomGen g => g -> [Food]
newFoodList g = ((x, y), Apple):(newFoodList g'')
  where
    (x, g')  = randomR range g
    (y, g'') = randomR range g'
    range = (0, baseBoardSize -1)

nextFood :: [Food] -> Snake -> (Food, [Food])
nextFood (f@(p, _):xs) s = if p `colidesWithSnake` s
  then nextFood xs s
  else (f, xs)

initialSnake :: Snake
initialSnake = [h, b, t]
  where
    sIndex = (div baseBoardSize 2)
    h = ((sIndex, sIndex), Right, Head1)
    b = ((sIndex - 1, sIndex), Right, Body1)
    t = ((sIndex - 2, sIndex), Right, Tail1)

checkDefeat :: Snake -> Bool
checkDefeat (((x, y), _, _):xs)
  =  x `outOfBounds` (0, baseBoardSize - 1)
  || y `outOfBounds` (0, baseBoardSize - 1)
  || (x, y) `colidesWithSnake` xs
  where
    outOfBounds a (l, h) = a < l || a > h

checkEaten :: Food -> Snake -> Bool
checkEaten (p, _) = (colidesWithSnake p) . pure . head

colidesWithSnake :: Position -> Snake -> Bool
colidesWithSnake p [] = False
colidesWithSnake p ((ps, _, _):xs) = if p == ps
  then True
  else p `colidesWithSnake` xs

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
