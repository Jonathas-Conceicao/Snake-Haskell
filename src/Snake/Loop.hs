module Snake.Loop
  ( gameLoop
  ) where

import Snake.GameState
import Snake.State.Match
import Snake.State.Menu
import Snake.State.Data

gameLoop :: Float -> GameState -> GameState
gameLoop t s = case currentScene s of
  MatchScene -> if defeated ms
    then s
    else s {matchState = matchLoop ms}
  MenuScene  -> menuLoop  s
  where
    ms = matchState s

menuLoop :: GameState -> GameState
menuLoop = id

matchLoop :: MatchState -> MatchState
matchLoop s = s
  { boardSize   = curBoardSize -- Should not change
  , interations = newInterations
  , foodList = newFL
  , snake = newSnake
  , eaten = newEaten
  , food  = newFood
  }
  where
    curBoardSize  = boardSize s
    curInterations = interations s
    curSnake = snake s
    curEaten = eaten s
    curSpeed = speed s
    curFood  = food s
    curFL    = foodList s

    newInterations = updateInterations curInterations
    newSnake = alterIf shouldMove curSnake (moveSnake newEaten)
    newEaten = hasEaten
    newFood  = if hasEaten then f  else curFood
    newFL    = if hasEaten then fl else curFL
    newSpeed = curSpeed

    (f, fl) = nextFood curFL curSnake
    updateInterations 0 = curSpeed
    updateInterations x = x - 1
    shouldMove = (curSpeed ==) curInterations
    hasEaten = checkEaten curFood newSnake

defeated :: MatchState -> Bool
defeated = checkDefeat . snake

alterIf :: Bool -> a -> (a -> a) -> a
alterIf True  a f = f a
alterIf False a _ = a
