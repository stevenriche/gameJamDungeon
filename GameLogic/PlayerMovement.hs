module GameLogic.PlayerMovement
( move'
, playerLocation'
, isPlayerDead'
) where

import GameLogic.Util

-- move function - takes a direction, then spits out a new map
move' :: [Char] -> [[Char]] -> Bool -> ([Char], [[Char]])
move' direction dungeonMap leaveMine
  | newSpace == 'O' || newSpace == '#'        = ("MON", ("Blocked!" : dungeonMap))
  | newSpace == 'M' || newSpace == '*'        = ("END", ("You have died. Sorry") : selfDestruction' dungeonMap player)
  | newSpace == 'x'                           = ("WIN", ("You found the exit! Press ENTER to move to the next level" : moveItem' dungeonMap player nextSpot))
  | leaveMine && (playerHasMines' dungeonMap) = ("MON", ("": moveItemWChar' (reduceMines' dungeonMap) player nextSpot '*'))
  | leaveMine                                 = ("MON", ("You are out of mines" : moveItem' dungeonMap player nextSpot))
  | otherwise                                 = ("MON", ("" : moveItem' dungeonMap player nextSpot))
  where player = playerLocation' dungeonMap
        nextSpot = playerWalkDistance' direction
        newSpace = nextSpace' dungeonMap player nextSpot

-- Returns a tuple of (Row index, Col index) of the player's current position
playerLocation' :: [[Char]] -> (Int, Int)
playerLocation' dungeonMap = characterLocation' dungeonMap '@'

-- returns a tuple of row and column offset given a direction to walk
playerWalkDistance' :: [Char] -> (Int, Int)
playerWalkDistance' "w" = (-1, 0)
playerWalkDistance' "d" = (0, 1)
playerWalkDistance' "s" = (1, 0)
playerWalkDistance' "a" = (0, -1)

-- returns true if character is not present on map (i.e. they died)
isPlayerDead' :: [[Char]] -> Bool
isPlayerDead' dungeonMap = characterNotPresent' dungeonMap '@'

-- Returns a list of mine locations with (row, column) tuples
getMineLocations' :: [[Char]] -> [(Int, Int)]
getMineLocations' dungeonMap =
  foldl (\l r -> l ++ (map (\c -> (r, c)) (findCols' (dungeonMap!!r) '*'))) [] (findRows' dungeonMap '[')

-- returns true if player is able to use a mine
playerHasMines' :: [[Char]] -> Bool
playerHasMines' dungeonMap = length (getMineLocations' dungeonMap) > 0

-- Returns a new dungeon map where the last mine is replaced by a '.'
reduceMines' :: [[Char]] -> [[Char]]
reduceMines' dungeonMap = selfDestruction' dungeonMap (last (getMineLocations' dungeonMap))
