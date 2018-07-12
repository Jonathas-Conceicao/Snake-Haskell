module Snake.GameState
  ( GameState(..)
  , Scene(..)
  , initialGameState
  ) where

import Snake.State.Match
import Snake.State.Menu
import Snake.State.Data

data GameState = GameState
  { currentScene :: Scene
  , matchState :: MatchState
  , menuState  :: MenuState
  , dataState  :: DataState
  }

data Scene = MatchScene | MenuScene

initialGameState :: GameState
initialGameState = GameState
  { currentScene = MatchScene
  , matchState = initialMatchState
  , menuState  = initialMenuState
  , dataState  = initialDataState
  }
