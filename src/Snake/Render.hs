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
renderMatch state = Gloss.pictures [ pBoard, pSnake, pFood ] -- , debug ]
  where
    ms = matchState state
    pBoard = renderBoard
    pSnake = draw $ snake ms
    pFood  = draw $ food ms
    -- debug = Gloss.text $ show $ interations ms

renderBoard :: Gloss.Picture
renderBoard = boardAsset

reposition :: Gloss.Picture -> Gloss.Picture -- TODO: Remove this or use relative values
reposition = Gloss.translate 0 0 -- (-640 / 2) (-672 / 2)
