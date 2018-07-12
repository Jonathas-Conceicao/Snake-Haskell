module Snake.State.Match
  ( MatchState(..)
  , initialMatchState
  , Board(..)
  , Line(..)
  , Slot(..)
  , Snake(..)
  , baseBoard
  ) where

data MatchState = MatchState Board

initialMatchState = MatchState $ baseBoard 5

data Snake
  = Head | Head2 | Tongue | Tail
  | Body230 | Body030
  | Body010 | Body120
  | Body130 | Body131
  | Body020 | Body021

data Slot  = Space | Wall | Food | PartOf Snake
type Line  = [Slot]
type Board = [Line]

instance Num Slot where
  negate      = undefined
  (+)         = undefined
  (*)         = undefined
  abs         = undefined
  signum      = undefined
  fromInteger 0 = Space
  fromInteger 1 = Wall
  fromInteger 2 = Food
  fromInteger n = PartOf $ fromInteger n-3

instance Num Snake where
  negate      = undefined
  (+)         = undefined
  (*)         = undefined
  abs         = undefined
  signum      = undefined
  fromInteger n = Head

baseBoard :: Int -> Board
baseBoard = addWalls . initBoard

initBoard :: Int -> Board
initBoard size = replicate size $ replicate size Space

addWalls :: Board -> Board
addWalls l@(x:xs) = [t] ++ (fmap addCorner l) ++ [t]
  where
    t = replicate ((length x) + 2) Wall
    addCorner k = [Wall] ++ k ++ [Wall]

