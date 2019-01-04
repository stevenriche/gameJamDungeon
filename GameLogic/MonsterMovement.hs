module GameLogic.MonsterMovement
( monsterMoves'
) where

import GameLogic.PlayerMovement
import GameLogic.Util

-- Part of the monster loop - moves the monsters and checks if the player died
monsterMoves' :: [[Char]] -> ([Char], [[Char]])
monsterMoves' dungeonMap
  | (isPlayerDead' newDungeonMap)    = ("END", ("You have died. Sorry": newDungeonMap))
  | otherwise                        = ("CON", ("" : newDungeonMap))
  where newDungeonMap = advanceMonsters' dungeonMap

-- Takes all monsters through a step and returns the redrawn map
advanceMonsters' :: [[Char]] -> [[Char]]
advanceMonsters' dungeonMap =
  let monsterList = findAllMonsters' dungeonMap
  in foldl (\dMap m -> advanceOneMonster' dMap m) dungeonMap monsterList

-- Finds all instances of all types of monsters (right now there is only 'M')
findAllMonsters' :: [[Char]] -> [(Char, Int, Int)]
findAllMonsters' dungeonMap = foldl (++) [] (map (\mType -> findMonsters' dungeonMap mType) ['M'])

-- Finds all instances of one type of monster
findMonsters' :: [[Char]] -> Char -> [(Char, Int, Int)]
findMonsters' dungeonMap monType =
  let rowsM = findRows' dungeonMap monType
  in foldl (++) [] (map (\r -> createMonsterTuples' monType r dungeonMap) rowsM)

-- Used to create tuples of (monster type, row index, column index) of found monsters
createMonsterTuples' :: Char -> Int -> [[Char]] -> [(Char, Int, Int)]
createMonsterTuples' monType rowIndex dungeonMap =
  map (\c -> (monType, rowIndex, c)) (findCols' (dungeonMap!!rowIndex) monType)

-- Moves one monster forward
advanceOneMonster' :: [[Char]] -> (Char, Int, Int) -> [[Char]]
advanceOneMonster' dungeonMap monster
  | (isPlayerDead' dungeonMap) = dungeonMap
  | monType == 'M'             = advanceOneMMonster' dungeonMap (row, col)
  | otherwise                  = dungeonMap
  where (monType, row, col) = monster

-- Specifically moves the 'M' type monster
-- The M monster tries to run directly at the player's position
-- A priority check is done to find what spaces it wants to go to most, then it finds the first
-- unblocked space and goes there.
advanceOneMMonster' :: [[Char]] -> (Int, Int) -> [[Char]]
advanceOneMMonster' dungeonMap coords
  | (abs (fst dCoords) <= 1 && abs (snd dCoords) <= 1) = moveItem' dungeonMap coords dCoords
  | (length openSpaces > 0)                            = moveItem' dungeonMap coords (openSpaces!!0)
  | otherwise                                          = dungeonMap
  where playerCoords = playerLocation' dungeonMap
        dCoords = ((fst playerCoords - fst coords), (snd playerCoords - snd coords))
        spacesPriorityOrder = mMonsterPriorityCheck' dCoords
        openSpaces = filter (\sp -> mMonsterBlockCheck' dungeonMap coords sp) spacesPriorityOrder

-- A method that determines what space is highest priority, depending on where the player is in
-- relation to the monster. If the player is SSE of the monster, the following is the priority list
-- of spaces the monster will attempt to go (the monster can move 2 spaces at once)
--                __
--             __|12|__
--          __|11| 9| 7|__
--         |10| 8| M| 5| 3|
--            | 6| 4| 2|
--               | 1|
-- If the player is some other direction from the monster, the following method will flip and
-- reorient the priority order as needed
mMonsterPriorityCheck' :: (Int, Int) -> [(Int, Int)]
mMonsterPriorityCheck' (r, c)
  | r > 0 && c > 0 && abs r > abs c = base -- SSE
  | r > 0 && c > 0                  = map (\(x, y) -> (y, x)) base -- SEE
  | r > 0 && abs r > abs c          = map (\(x, y) -> (x, (-1 * y))) base -- SSW
  | r > 0                           = map (\(x, y) -> (y, (-1 * x))) base -- SWW
  | c > 0 && abs r > abs c          = map (\(x, y) -> ((-1 * x) , y)) base -- NNE
  | c > 0                           = map (\(x, y) -> ((-1 * y), x)) base -- NEE
  | abs r > abs c                   = map (\(x, y) -> ((-1 * x), (-1 * y))) base -- NNW
  | otherwise                       = map (\(x, y) -> ((-1 * y), (-1 * x))) base -- NWW
  where base = [(2, 0), (1, 1), (0, 2), (1, 0), (0, 1), (1, -1), (-1, 1), (0, -1), (-1, 0), (0, -2), (-1, -1), (-2, 0)]

-- This method checks if the path is blocked for an 'M' monster. Since this monster can move two
-- spaces at once, we have to do a check for both the space it lands and the space in between
mMonsterBlockCheck' :: [[Char]] -> (Int, Int) -> (Int, Int) -> Bool
mMonsterBlockCheck' dungeonMap coords (r, c)
  | abs r < 2 && abs c < 2 = isSpaceOpen' dungeonMap coords (r, c)
  | abs r < 2              = (isSpaceOpen' dungeonMap coords (r, (quot c 2))) && (isSpaceOpen' dungeonMap coords (r, c))
  | otherwise              = (isSpaceOpen' dungeonMap coords ((quot r 2), c)) && (isSpaceOpen' dungeonMap coords (r, c))
