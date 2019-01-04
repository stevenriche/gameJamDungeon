module GameLogic.PlayerMovement
( move'
, playerLocation'
, isPlayerDead'
) where

import GameLogic.Util

-- move function - takes a direction, then spits out a new map
move' :: [Char] -> [[Char]] -> ([Char], [[Char]])
move' direction dungeonMap
  | newSpace == 'O' || newSpace == '#'    = ("MON", ("Blocked!" : dungeonMap))
  | newSpace == 'M'                       = ("END", ("You have died. Sorry") : selfDestruction' dungeonMap player)
  | newSpace == 'x'                       = ("WIN", ("You found the exit! Press ENTER to move to the next level" : moveItem' dungeonMap player nextSpot))
  | otherwise                             = ("MON", ("" : moveItem' dungeonMap player nextSpot))
  where player = playerLocation' dungeonMap
        nextSpot = playerWalkDistance' direction
        newSpace = nextSpace' dungeonMap player nextSpot

-- Returns a tuple of (Row index, Col index) of the player's current position
playerLocation' :: [[Char]] -> (Int, Int)
playerLocation' dungeonMap =
  let rowIndex = (findRows' dungeonMap '@') !! 0
  in (rowIndex, ((findCols' (dungeonMap !! rowIndex) '@') !! 0))

-- returns a tuple of row and column offset given a direction to walk
playerWalkDistance' :: [Char] -> (Int, Int)
playerWalkDistance' "w" = (-1, 0)
playerWalkDistance' "d" = (0, 1)
playerWalkDistance' "s" = (1, 0)
playerWalkDistance' "a" = (0, -1)

-- returns true if character is not present on map (i.e. they died)
isPlayerDead' :: [[Char]] -> Bool
isPlayerDead' dungeonMap = characterNotPresent' dungeonMap '@'
