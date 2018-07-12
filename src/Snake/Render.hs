module Snake.Render
  ( renderGame
  ) where

import Snake.GameState
import Snake.State.Data
import Snake.State.Menu
import Snake.State.Match
import Snake.Resource

import qualified Graphics.Gloss.Game as Gloss

renderGame :: GameState -> Gloss.Picture
renderGame s = reposition $ case currentScene s of
  MatchScene -> renderMatch s
  MenuScene -> renderMenu s

renderMenu :: GameState -> Gloss.Picture
renderMenu s = Gloss.pictures []

renderMatch :: GameState -> Gloss.Picture
renderMatch state = Gloss.pictures [ pBoard ]
  where
    pBoard = renderBoard $ matchBoard $ matchState state

renderBoard :: Board -> Gloss.Picture
renderBoard board = draw board

reposition :: Gloss.Picture -> Gloss.Picture
reposition = Gloss.translate (-896 / 2) (-672 / 2)
