{-# LANGUAGE FlexibleInstances #-}

module Snake.Resource
  ( Drawn(..)
  , boardAsset
  ) where

import Snake.State.Match

import qualified Graphics.Gloss.Game as Gloss

class Drawn a where
  draw :: a -> Gloss.Picture
  drawAt :: (Int, Int) -> a -> Gloss.Picture
  drawAt (x, y) a = Gloss.translate (toSlot x) (toSlot y) $ draw a

instance Drawn Snake where
  draw = Gloss.pictures . map draw

instance Drawn SnakePart where
  draw (p, _, s) = drawAt p s

instance Drawn SnakeSlot where
  draw = snakeAsset

toSlot :: Int -> Float
toSlot = (subtract 272) . (*) slotSize . fromIntegral
-- 320 - 32 - 16
-- backgroundSize - slotSize - halfSlotSize

slotSize :: Float
slotSize = 32

steps = [0, slotSize ..]

-- positionInLine :: [Gloss.Picture] -> [Gloss.Picture]
-- positionInLine = positionInLine' . zip steps
--   where
--     positionInLine' = map (\(x, a) -> Gloss.translate x 0 a)

-- positionInColumn :: [Gloss.Picture] -> [Gloss.Picture]
-- positionInColumn = positionInColumn' . zip steps
--   where
--     positionInColumn' = map (\(y, a) -> Gloss.translate 0 y a)

snakeAsset :: SnakeSlot -> Gloss.Picture
snakeAsset Head0   = Gloss.png "assets/png/snake/Head0.png"
snakeAsset Head1   = Gloss.png "assets/png/snake/Head1.png"
snakeAsset Head2   = Gloss.png "assets/png/snake/Head2.png"
snakeAsset Head3   = Gloss.png "assets/png/snake/Head3.png"
snakeAsset THead0  = Gloss.png "assets/png/snake/THead0.png"
snakeAsset THead1  = Gloss.png "assets/png/snake/THead1.png"
snakeAsset THead2  = Gloss.png "assets/png/snake/THead2.png"
snakeAsset THead3  = Gloss.png "assets/png/snake/THead3.png"
snakeAsset Tongue0 = Gloss.png "assets/png/snake/Tongue0.png"
snakeAsset Tongue1 = Gloss.png "assets/png/snake/Tongue1.png"
snakeAsset Tongue2 = Gloss.png "assets/png/snake/Tongue2.png"
snakeAsset Tongue3 = Gloss.png "assets/png/snake/Tongue3.png"
snakeAsset Tail0   = Gloss.png "assets/png/snake/Tail0.png"
snakeAsset Tail1   = Gloss.png "assets/png/snake/Tail1.png"
snakeAsset Tail2   = Gloss.png "assets/png/snake/Tail2.png"
snakeAsset Tail3   = Gloss.png "assets/png/snake/Tail3.png"
snakeAsset Body0   = Gloss.png "assets/png/snake/Body0.png"
snakeAsset Body1   = Gloss.png "assets/png/snake/Body1.png"
snakeAsset Body2   = Gloss.png "assets/png/snake/Body2.png"
snakeAsset Body3   = Gloss.png "assets/png/snake/Body3.png"
snakeAsset Curve0  = Gloss.png "assets/png/snake/Curve0.png"
snakeAsset Curve1  = Gloss.png "assets/png/snake/Curve1.png"
snakeAsset Curve2  = Gloss.png "assets/png/snake/Curve2.png"
snakeAsset Curve3  = Gloss.png "assets/png/snake/Curve3.png"

boardAsset :: Gloss.Picture
boardAsset = Gloss.png "assets/png/Background.png"
