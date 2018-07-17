module Main
  (main
  ) where

import Snake.Render
import Snake.GameState
import Snake.Loop
import Snake.Input

import System.Random

import qualified Graphics.Gloss.Game as Gloss

screenWidth, screenHeight :: Int
(screenWidth, screenHeight) = (640, 640)

window :: Gloss.Display
window = (Gloss.InWindow "Snake-Haskell" (screenWidth, screenHeight) (0, 0))

background :: Gloss.Color
background = Gloss.white

main :: IO ()
main = do
  gen <- newStdGen
  Gloss.play
    window
    background
    16
    (initialGameState gen)
    renderGame gameInput
    [gameLoop]        
