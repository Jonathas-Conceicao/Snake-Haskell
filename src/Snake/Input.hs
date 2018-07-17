module Snake.Input
  ( gameInput
  ) where

import Prelude hiding (Either(..))

import Snake.GameState
import Snake.State.Data
import Snake.State.Menu
import Snake.State.Match hiding (Direction(..))
import qualified Snake.State.Match as Match (Direction(..))

import qualified Graphics.Gloss.Game as Gloss

data Input
  = Up | Right | Down | Left
  | Accept | Cancel

gameInput :: Gloss.Event -> GameState -> GameState
gameInput e s = case currentScene s of
  MatchScene -> s {matchState = matchInput e $ matchState s}
  MenuScene  -> menuInput e s

menuInput :: Gloss.Event -> GameState -> GameState
menuInput e = id

matchInput :: Gloss.Event -> MatchState -> MatchState
matchInput e s = s
  { snake = snakeInput e $ snake s
  }

snakeInput :: Gloss.Event -> Snake -> Snake
snakeInput e s
  | isEvent e Up    = tryMove s Up
  | isEvent e Right = tryMove s Right
  | isEvent e Down  = tryMove s Down
  | isEvent e Left  = tryMove s Left
  | otherwise = s

tryMove :: Snake -> Input -> Snake
tryMove ((p, d, s):xs) i = ((p, tryMove' s i, s):xs)
  where
    tryMove' Head0 Down  = d
    tryMove' Head1 Left  = d
    tryMove' Head2 Up    = d
    tryMove' Head3 Right = d
    tryMove' _ i = trans i
    trans Up = Match.Up
    trans Right = Match.Right
    trans Down = Match.Down
    trans Left = Match.Left

isEvent :: Gloss.Event -> Input -> Bool
isEvent (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyUp) _ _ _) Up = True
isEvent (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyRight) _ _ _) Right = True
isEvent (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyDown) _ _ _) Down = True
isEvent (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyLeft) _ _ _) Left = True

isEvent (Gloss.EventKey (Gloss.Char 'W') _ _ _) Up = True
isEvent (Gloss.EventKey (Gloss.Char 'D') _ _ _) Right = True
isEvent (Gloss.EventKey (Gloss.Char 'S') _ _ _) Down = True
isEvent (Gloss.EventKey (Gloss.Char 'A') _ _ _) Left = True

isEvent (Gloss.EventKey (Gloss.Char 'J') _ _ _) Accept = True
isEvent (Gloss.EventKey (Gloss.Char 'K') _ _ _) Cancel = True

isEvent _ _ = False
