module Main
  (main
  ) where

import Snake.Render
import Snake.GameState
import Snake.Loop
import Snake.Input

import qualified Graphics.Gloss.Game as Gloss

screenWidth, screenHeight :: Int
(screenWidth, screenHeight) = (640, 640)

window :: Gloss.Display
window = (Gloss.InWindow "Snake-Haskell" (screenWidth, screenHeight) (0, 0))

background :: Gloss.Color
background = Gloss.white

main :: IO ()
main = Gloss.play window background 60 initialGameState renderGame gameInput [gameLoop]

    -- draw (World {creeperPos = (x, y)}) 
    --   = pictures [ translate x y (scale 0.5 0.5 creeperSprite)
    --              , translate (fst platformPos) (snd platformPos) (scale 0.5 0.5 platformSprite)
    --              ]
    
    -- handle (EventKey (Char 'a') Down _ _)            world = world {creeperPos = moveX (creeperPos world) (-10)}
    -- handle (EventKey (Char 'd') Down _ _)            world = world {creeperPos = moveX (creeperPos world) 10}
    -- handle (EventKey (SpecialKey KeySpace) Down _ _) world = world {creeperVelocity = 10}
    -- handle _event                         world            = world
        
