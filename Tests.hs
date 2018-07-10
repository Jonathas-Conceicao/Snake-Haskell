module Tests where

import GameState
import Display

map3 :: Board
map3 = [[1,1,1,1,1]
       ,[1,0,7,0,1]
       ,[1,0,5,0,1]
       ,[1,0,5,5,1]
       ,[1,1,1,1,1]]

boardExample :: Board
boardExample = baseBoard 10
