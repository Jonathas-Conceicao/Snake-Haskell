{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Foreign.C.Types
import SDL.Vect (V2)

import qualified SDL.Vect as SDLVec
import qualified SDL

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "SDL Tutorial" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  SDL.showWindow window
  screenSurface <- SDL.getWindowSurface window

  xOut <- createRGBSurface (V2 screenWidth screenHeight) SDL.Index8

  let
    loop = do
      events <- SDL.pollEvents
      let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

      SDL.surfaceBlit xOut Nothing screenSurface Nothing
      SDL.updateWindowSurface window

      unless quit loop

  loop

  SDL.freeSurface xOut
  SDL.destroyWindow window
  SDL.quit
