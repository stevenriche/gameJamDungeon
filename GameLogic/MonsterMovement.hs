module GameLogic.MonsterMovement
( monsterMoves'
) where

import GameLogic.PlayerMovement
import GameLogic.Util
import GameLogic.Monsters.MMonster
import GameLogic.Monsters.BMonster

-- Part of the monster loop - moves the monsters and checks if the player died
monsterMoves' :: [[Char]] -> ([Char], [[Char]])
monsterMoves' dungeonMap
  | (isPlayerDead' newDungeonMap)    = ("END", ("You have died. Sorry" : newDungeonMap))
  | (characterNotPresent' newDungeonMap 'x') = ("END", ("The exit is blocked. Game Over." : newDungeonMap))
  | otherwise                        = ("CON", ("" : newDungeonMap))
  where newDungeonMap = advanceMonsters' dungeonMap

-- Takes all monsters through a step and returns the redrawn map
advanceMonsters' :: [[Char]] -> [[Char]]
advanceMonsters' dungeonMap =
  let monsterList = findAllMonsters' dungeonMap
  in foldl (\dMap m -> advanceOneMonster' dMap m) dungeonMap monsterList

-- Finds all instances of all types of monsters (right now there is only 'M' and 'B')
findAllMonsters' :: [[Char]] -> [(Char, Int, Int)]
findAllMonsters' dungeonMap = foldl (++) [] (map (\mType -> findMonsters' dungeonMap mType) ['M', 'B'])

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
  | monType == 'B'             = advanceOneBMonster' dungeonMap (row, col)
  | otherwise                  = dungeonMap
  where (monType, row, col) = monster
