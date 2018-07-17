module Snake.GameState
  ( GameState(..)
  , Scene(..)
  , initialGameState
  ) where

import Snake.State.Match
import Snake.State.Menu
import Snake.State.Data

import System.Random

data GameState = GameState
  { currentScene :: Scene
  , matchState :: MatchState
  , menuState  :: MenuState
  , dataState  :: DataState
  }

data Scene = MatchScene | MenuScene

initialGameState :: RandomGen g => g -> GameState
initialGameState g = GameState
  { currentScene = MatchScene
  , matchState = initialMatchState g
  , menuState  = initialMenuState
  , dataState  = initialDataState
  }
