{-# LANGUAGE FlexibleInstances #-}

module Snake.Resource
  ( Drawn(..)
  , Slot(..)
  , Line(..)
  , Board(..)
  ) where

import Snake.State.Match

import qualified Graphics.Gloss.Game as Gloss

class Drawn a where
  draw :: a -> Gloss.Picture
  drawAt :: Float -> Float -> a -> Gloss.Picture
  drawAt x y a = Gloss.translate x y $ draw a

instance Drawn Slot where
  draw = Gloss.png . slotAsset

instance Drawn Line where
  draw = Gloss.pictures . positionInLine . map draw

instance Drawn Board where
  draw = Gloss.pictures . positionInColumn . map draw

slotSize :: Float
slotSize = 32

steps = [0, slotSize ..]

positionInLine :: [Gloss.Picture] -> [Gloss.Picture]
positionInLine = positionInLine' . zip steps
  where
    positionInLine' = map (\(x, a) -> Gloss.translate x 0 a)

positionInColumn :: [Gloss.Picture] -> [Gloss.Picture]
positionInColumn = positionInColumn' . zip steps
  where
    positionInColumn' = map (\(y, a) -> Gloss.translate 0 y a)

slotAsset :: Slot -> String
slotAsset (Menu (Top Fst)) = "assets/png/nicepatch/arenaT0.png"
slotAsset (Menu (Top Snd)) = "assets/png/nicepatch/arenaT1.png"
slotAsset (Menu (Top Trd)) = "assets/png/nicepatch/arenaT2.png"
slotAsset (Menu (Mid Fst)) = "assets/png/nicepatch/arenaM0.png"
slotAsset (Menu (Mid Snd)) = "assets/png/nicepatch/arenaM1.png"
slotAsset (Menu (Mid Trd)) = "assets/png/nicepatch/arenaM2.png"
slotAsset (Menu (Bot Fst)) = "assets/png/nicepatch/arenaB0.png"
slotAsset (Menu (Bot Snd)) = "assets/png/nicepatch/arenaB1.png"
slotAsset (Menu (Bot Trd)) = "assets/png/nicepatch/arenaB2.png"
slotAsset _ = "assets/png/invalid.png"
