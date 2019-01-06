module GameLogic.Util
( findRows'
, findCols'
, characterNotPresent'
, nextSpace'
, getCharAtSpace'
, characterLocation'
, isSpaceOpen'
, moveItem'
, moveItemWChar'
, selfDestruction'
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

-- Returns the indices of the rows where the given character is located
findRows' :: [[Char]] -> Char -> [Int]
findRows' dungeonMap character = findIndices (elem character) dungeonMap

-- Returns the indices of the columns where the given character is located
findCols' :: [Char] -> Char -> [Int]
findCols' dungeonMapRow character = elemIndices character dungeonMapRow

-- Returns true if character was not found at all
characterNotPresent' :: [[Char]] -> Char -> Bool
characterNotPresent' dungeonMap character = isNothing $ findIndex (elem character) dungeonMap

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

-- Returns a tuple of (Row index, Col index) of the given character's current position
characterLocation' :: [[Char]] -> Char -> (Int, Int)
characterLocation' dungeonMap character =
  let rowIndex = (findRows' dungeonMap character) !! 0
  in (rowIndex, ((findCols' (dungeonMap !! rowIndex) character) !! 0))

-- Returns true if the player (or monster) is moving onto an available space
isSpaceOpen' :: [[Char]] -> (Int, Int) -> (Int, Int) -> Bool
isSpaceOpen' dungeonMap currentSpace newSpaceOffset =
  let newChar = getCharAtSpace' dungeonMap currentSpace newSpaceOffset
  in (newChar /= 'O' && newChar /= '#' && newChar /= 'M' )

-- Function that calls the user movement and makes a new map, replacing old character with
-- the one given
moveItemWChar' :: [[Char]] -> (Int, Int) -> (Int, Int) -> Char -> [[Char]]
moveItemWChar' dungeonMap item nextSpot replacingChar =
  let newItemRow = fst item + fst nextSpot
      newItemCol = snd item + snd nextSpot
      itemChar = getCharAtSpace' dungeonMap item (0, 0)
  in redrawMap' (redrawMap' dungeonMap item replacingChar) (newItemRow, newItemCol) itemChar

-- Function that calls the user movement and makes a new map
moveItem' :: [[Char]] -> (Int, Int) -> (Int, Int) -> [[Char]]
moveItem' dungeonMap item nextSpot = moveItemWChar' dungeonMap item nextSpot '.'

-- Function that removes the player if they kill themselves
selfDestruction' :: [[Char]] -> (Int, Int) -> [[Char]]
selfDestruction' dungeonMap item = redrawMap' dungeonMap item '.'
