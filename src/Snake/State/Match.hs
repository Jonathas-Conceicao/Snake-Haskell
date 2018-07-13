module Snake.State.Match
  ( MatchState(..)
  , initialMatchState
  , Board(..)
  , Line(..)
  , Slot(..)
  , Snake(..)
  , NinePatch(..)
  , NPLine(..)
  , Position(..)
  , baseBoard
  , moveSnake
  ) where

data MatchState = MatchState
  { matchBoard :: Board
  , interations :: Int
  , snake :: [(Position, Snake)]
  }

data Snake
  = Head | Head2 | Tongue | Tail
  | Body230 | Body030
  | Body010 | Body120
  | Body130 | Body131
  | Body020 | Body021

data Slot  = Menu NinePatch | Food | PartOf Snake

data NinePatch
  = Top NPLine 
  | Mid NPLine
  | Bot NPLine
data NPLine = Fst | Snd | Trd

type Line  = [Slot]
type Board = [Line]
type Position = (Int, Int)

instance Num Slot where
  negate = undefined
  (+)    = undefined
  (*)    = undefined
  abs    = undefined
  signum = undefined
  fromInteger 0 = Menu $ Top Fst
  fromInteger 1 = Menu $ Top Snd
  fromInteger 2 = Menu $ Top Trd
  fromInteger 3 = Menu $ Mid Fst
  fromInteger 4 = Menu $ Mid Snd
  fromInteger 5 = Menu $ Mid Trd
  fromInteger 6 = Menu $ Bot Fst
  fromInteger 7 = Menu $ Bot Snd
  fromInteger 8 = Menu $ Bot Trd
  fromInteger 9 = Food
  fromInteger n = PartOf $ fromInteger n-3

instance Num Snake where
  negate = undefined
  (+)    = undefined
  (*)    = undefined
  abs    = undefined
  signum = undefined
  fromInteger n = Head

baseBoard :: Int -> Board
baseBoard = addWalls . initBoard

initBoard :: Int -> Board
initBoard size = replicate size $ replicate size $ Menu $ Mid Snd

addWalls :: Board -> Board
addWalls l@(x:_) = [top] ++ midList ++ [bot]
  where
    midList = fmap addCorner l
    addCorner k = [Menu $ Mid Fst] ++ k ++ [Menu $ Mid Trd]
    top = [Menu $ Top Fst] ++ (replicate (length x) $ Menu $ Top Snd) ++ [Menu $ Top Trd]
    bot = [Menu $ Bot Fst] ++ (replicate (length x) $ Menu $ Bot Snd) ++ [Menu $ Bot Trd]

initialMatchState :: MatchState
initialMatchState = MatchState
  { matchBoard = baseBoard 20
  , interations = 60
  , snake = [((10, 10), Head)]
  }

moveSnake :: [(Position, Snake)] -> [(Position, Snake)]
moveSnake [] = []
moveSnake (((x,y), s):xs) = (((x+1,y), s):xs)
