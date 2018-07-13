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
renderMatch state = Gloss.pictures [ pBoard, pSnake, debug ]
  where
    ms = matchState state
    pBoard = renderBoard $ matchBoard ms
    pSnake = Gloss.pictures $ renderSnake $ snake ms
    debug = Gloss.text $ show $ interations ms

renderBoard :: Board -> Gloss.Picture
renderBoard board = draw board

renderSnake :: [(Position, Snake)] -> [Gloss.Picture]
renderSnake = map renderSnake'
  where
    renderSnake' (p, s) = drawAt p s

reposition :: Gloss.Picture -> Gloss.Picture
reposition = Gloss.translate (-896 / 2) (-672 / 2)
