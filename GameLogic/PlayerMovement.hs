module GameLogic.PlayerMovement
( move'
) where

import GameLogic.Util

-- move function - takes a direction, then spits out a new map
move' :: [Char] -> [[Char]] -> ([Char], [[Char]])
move' direction dungeonMap
  | newSpace == 'O' || newSpace == '#'    = ("CONTINUE", ("Blocked!" : dungeonMap))
  | newSpace == 'x'                       = ("WIN", ("You found the exit! Press ENTER to move to the next level" : movePlayer' dungeonMap player nextSpot))
  | otherwise                             = ("CONTINUE", ("" : movePlayer' dungeonMap player nextSpot))
  where player = playerLocation' dungeonMap
        nextSpot = playerWalkDistance' direction
        newSpace = nextSpace' dungeonMap player nextSpot

-- Function that calls the user movement and makes a new map
movePlayer' :: [[Char]] -> (Int, Int) -> (Int, Int) -> [[Char]]
movePlayer' dungeonMap player nextSpot =
  let newPlayerRow = fst player + fst nextSpot
      newPlayerCol = snd player + snd nextSpot
  in redrawMap' (redrawMap' dungeonMap player '.') (newPlayerRow, newPlayerCol) '@'

-- Returns a tuple of (Row index, Col index) of the player's current position
playerLocation' :: [[Char]] -> (Int, Int)
playerLocation' dungeonMap = (findRow' dungeonMap '@', findCol' dungeonMap '@')

-- returns a tuple of row and column offset given a direction to walk
playerWalkDistance' :: [Char] -> (Int, Int)
playerWalkDistance' "w" = (-1, 0)
playerWalkDistance' "d" = (0, 1)
playerWalkDistance' "s" = (1, 0)
playerWalkDistance' "a" = (0, -1)
