module Snake.Input
  ( gameInput
  ) where

import Prelude hiding (Either(..))
import System.Random

import Snake.State hiding (Direction(..))
import qualified Snake.State as State (Direction(..))

import qualified Graphics.Gloss.Game as Gloss

data Input
  = Up | Right | Down | Left
  | Accept | Close

gameInput :: Gloss.Event -> GameState -> GameState
gameInput e s = if defeated s
  then defeatedInput e s
  else s { snake = snakeInput e $ snake s }

defeatedInput :: Gloss.Event -> GameState -> GameState
defeatedInput e s = if isEvent e Accept
  then (initialGameState (newGen s)) { defeated = False }
  else s

newGen :: GameState -> StdGen
newGen = mkStdGen . (uncurry (-)) . fst . food

snakeInput :: Gloss.Event -> Snake -> Snake
snakeInput e s
  | isEvent e Up    = tryMove s Up
  | isEvent e Right = tryMove s Right
  | isEvent e Down  = tryMove s Down
  | isEvent e Left  = tryMove s Left
  | isEvent e Close = closeAll
  | otherwise = s

closeAll = error "Close all!"

tryMove :: Snake -> Input -> Snake
tryMove ((p, d, s):xs) i = ((p, tryMove' s i, s):xs)
  where
    tryMove' Head0 Down  = d
    tryMove' Head1 Left  = d
    tryMove' Head2 Up    = d
    tryMove' Head3 Right = d
    tryMove' _ i = trans i
    trans Up = State.Up
    trans Right = State.Right
    trans Down = State.Down
    trans Left = State.Left

isEvent :: Gloss.Event -> Input -> Bool
isEvent (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyUp) _ _ _) Up = True
isEvent (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyRight) _ _ _) Right = True
isEvent (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyDown) _ _ _) Down = True
isEvent (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyLeft) _ _ _) Left = True

isEvent (Gloss.EventKey (Gloss.Char 'w') _ _ _) Up = True
isEvent (Gloss.EventKey (Gloss.Char 'd') _ _ _) Right = True
isEvent (Gloss.EventKey (Gloss.Char 's') _ _ _) Down = True
isEvent (Gloss.EventKey (Gloss.Char 'a') _ _ _) Left = True


isEvent (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyEsc)   _ _ _) Close = True
isEvent (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyEnter) _ _ _) Accept = True
isEvent (Gloss.EventKey (Gloss.SpecialKey Gloss.KeySpace) _ _ _) Accept = True

isEvent _ _ = False
