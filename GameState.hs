module GameState
  ( Board(..)
  , Line(..)
  , Slot(..)
  , Snake(..)
  , baseBoard
  ) where

import Prelude hiding (Either(..))

data Snake = Head | Up | Down | Left | Right
data Slot  = Space | Wall | Food | S Snake
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
  fromInteger 3 = S Up
  fromInteger 4 = S Down
  fromInteger 5 = S Left
  fromInteger 6 = S Right
  fromInteger 7 = S Head

baseBoard :: Int -> Board
baseBoard = addWalls . initBoard

initBoard :: Int -> Board
initBoard size = replicate size $ replicate size Space

addWalls :: Board -> Board
addWalls l@(x:xs) = [t] ++ (fmap addCorner l) ++ [t]
  where
    t = replicate ((length x) + 2) Wall
    addCorner k = [Wall] ++ k ++ [Wall]

