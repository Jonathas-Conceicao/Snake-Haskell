module Snake.GameState
  ( GameState(..)
  , initialGameState
  ) where

import Snake.State.Match
import Snake.State.Menu
import Snake.State.Data

data GameState = GameState
  { matchState :: MatchState
  , menuState  :: MenuState
  , dataState  :: DataState
  }

initialGameState :: GameState
initialGameState = GameState
  { matchState = initialMatchState
  , menuState  = initialMenuState
  , dataState  = initialDataState
  }
