module Snake.Render
  ( renderGame
  ) where

import Snake.State
import Snake.Resource

import qualified Graphics.Gloss.Game as Gloss

renderGame :: GameState -> Gloss.Picture
renderGame s = Gloss.pictures [ pBoard, pSnake, pFood, pScore ]
  where
    pBoard = renderBoard
    pSnake = draw $ snake s
    pFood  = draw $ food s
    pScore = draw $ score s

renderBoard :: Gloss.Picture
renderBoard = boardAsset
