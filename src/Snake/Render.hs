module Snake.Render (
  renderGame
  ) where

import Snake.GameState
import Snake.State.Data
import Snake.State.Menu
import Snake.State.Match

import qualified Graphics.Gloss.Game as Gloss

renderGame :: GameState -> Gloss.Picture
renderGame s = Gloss.pictures []
