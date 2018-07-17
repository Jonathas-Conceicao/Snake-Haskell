module Snake.Loop
  ( gameLoop
  ) where

import Snake.GameState
import Snake.State.Match
import Snake.State.Menu
import Snake.State.Data

gameLoop :: Float -> GameState -> GameState
gameLoop t s = case currentScene s of
  MatchScene -> matchLoop s
  MenuScene  -> menuLoop  s

menuLoop :: GameState -> GameState
menuLoop = id

matchLoop :: GameState -> GameState
matchLoop s = s
  { currentScene = currentScene s
  , matchState = updateMatchLoop $ matchState s
  , menuState = menuState s
  , dataState = dataState s
  }

updateMatchLoop :: MatchState -> MatchState
updateMatchLoop s = s
  { boardSize   = curBoardSize -- Should not change
  , interations = newInterations
  , snake = newSnake
  , eaten = newEaten
  }
  where
    curBoardSize  = boardSize s
    curInterations = interations s
    curSnake = snake s
    curEaten = eaten s

    newInterations = updateInterations curInterations
    newSnake = alterIf shouldMove curSnake (moveSnake curEaten)
    newEaten = curEaten

    updateInterations 0 = 60
    updateInterations x = x - 1
    shouldMove = (60 ==) curInterations

alterIf :: Bool -> a -> (a -> a) -> a
alterIf True  a f = f a
alterIf False a _ = a
