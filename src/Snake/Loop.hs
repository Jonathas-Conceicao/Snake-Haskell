module Snake.Loop
  ( gameLoop
  ) where

import Snake.State

gameLoop :: Float -> GameState -> GameState
gameLoop f s = if defeated s
  then s
  else matchLoop s

matchLoop :: GameState -> GameState
matchLoop s = s
  { boardSize   = curBoardSize
  , interations = newInterations
  , defeated = newDefeated
  , score = newScore
  , foodList = newFL
  , snake = newSnake
  , eaten = newEaten
  , food  = newFood
  }
  where
    curBoardSize  = boardSize s
    curInterations = interations s
    curDefeated = defeated s
    curScore = score s
    curSnake = snake s
    curEaten = eaten s
    curSpeed = speed s
    curFood  = food s
    curFL    = foodList s

    newDefeated = if shouldMove then checkDefeat newSnake else curDefeated
    newInterations = updateInterations curInterations
    newSnake = alterIf shouldMove curSnake (moveSnake newEaten)
    newEaten = hasEaten
    newScore = if hasEaten then curScore + 1 else curScore
    newFood  = if hasEaten then f  else curFood
    newFL    = if hasEaten then fl else curFL
    newSpeed = curSpeed

    (f, fl) = nextFood curFL curSnake
    updateInterations 0 = curSpeed
    updateInterations x = x - 1
    shouldMove = (curSpeed ==) curInterations
    hasEaten = checkEaten curFood newSnake

alterIf :: Bool -> a -> (a -> a) -> a
alterIf True  a f = f a
alterIf False a _ = a
