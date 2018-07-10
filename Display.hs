module Display
  ( printBoard
  , printLine
  , printSlot
  ) where

import qualified GameState as GS

printBoard :: GS.Board -> IO ()
printBoard = mapM_ printLine

printLine :: GS.Line -> IO ()
printLine b = mapM_ printSlot b >> putStr "\n"

printSlot :: GS.Slot -> IO ()
printSlot = putStr . slotValue

slotValue :: GS.Slot -> String
slotValue GS.Space = "  "
slotValue GS.Wall  = "**"
slotValue GS.Food  = "xx"
slotValue (GS.S x) = "@@"


