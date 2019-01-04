module GameLogic.Util
( redrawMap'
, findRow'
, findCol'
, nextSpace'
) where

import Data.List
import Data.Maybe

-- Return a new map
-- dM = dungeonMap, nS = new space, nC = new character
redrawMap' :: [[Char]] -> (Int, Int) -> Char -> [[Char]]
redrawMap' dM nS nC =
  replaceAtIndex' (fst nS) (replaceAtIndex' (snd nS) nC (dM!!(fst nS))) dM

-- Utility function that returns a new list with a given index replaced
replaceAtIndex' :: Int -> a -> [a] -> [a]
replaceAtIndex' n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

-- Returns the index of the row where the given character is located
findRow' :: [[Char]] -> Char -> Int
findRow' dungeonMap character = fromJust $ findIndex (elem character) dungeonMap

-- Returns the index of the column where the given character is located
findCol' :: [[Char]] -> Char -> Int
findCol' dungeonMap character =
  fromJust $ elemIndex character (dungeonMap!!(findRow' dungeonMap character))

-- Returns the character on the map relative to the user space and the distance
-- dungeonMap: List of Strings representing ASCII dungeon map
-- currentSpace: Int tuple location of current space (row index, column index)
-- newSpaceOffset: Int tuple offset of space to check (row offset, column offset)
nextSpace' :: [[Char]] -> (Int, Int) -> (Int, Int) -> Char
nextSpace' dungeonMap currentSpace newSpaceOffset =
  if (outOfBounds' dungeonMap currentSpace newSpaceOffset)
    then 'O'
    else (getCharAtSpace' dungeonMap currentSpace newSpaceOffset)

-- Returns true if new space is out of bounds of map
-- dungeonMap: List of Strings representing ASCII dungeon map
-- currentSpace: Int tuple location of current space (row index, column index)
-- newSpaceOffset: Int tuple offset of space to check (row offset, column offset)
outOfBounds' :: [[Char]] -> (Int, Int) -> (Int, Int) -> Bool
outOfBounds' dungeonMap currentSpace newSpaceOffset
  | newRow < 0                              = True
  | newRow >= (length dungeonMap)           = True
  | newCol < 0                              = True
  | newCol >= (length (dungeonMap!!newRow)) = True
  | otherwise                               = False
  where newRow = fst currentSpace + fst newSpaceOffset
        newCol = snd currentSpace + snd newSpaceOffset

-- Returns the character currently at the new position
-- dungeonMap: List of Strings representing ASCII dungeon map
-- currentSpace: Int tuple location of current space (row index, column index)
-- newSpaceOffset: Int tuple offset of space to check (row offset, column offset)
getCharAtSpace' :: [[Char]] -> (Int, Int) -> (Int, Int) -> Char
getCharAtSpace' dungeonMap currentSpace newSpaceOffset =
  let newRow = fst currentSpace + fst newSpaceOffset
      newCol = snd currentSpace + snd newSpaceOffset
  in (dungeonMap !! newRow) !! newCol
