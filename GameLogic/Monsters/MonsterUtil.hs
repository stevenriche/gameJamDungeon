module GameLogic.Monsters.MonsterUtil
( moveIfNotMined'
, monsterPriorityCheck'
) where

import GameLogic.Util

-- Checks first if the monster has passed over a mine, and destroys the monster. If not, moves
moveIfNotMined' :: [[Char]] -> (Int, Int) -> (Int, Int) -> [[Char]]
moveIfNotMined' dungeonMap (r, c) (dr, dc)
  | (getCharAtSpace' dungeonMap (r, c) (dr, dc) == '*')                        =
    selfDestruction' (selfDestruction' dungeonMap (r, c)) ((r + dr), (c + dc))
  | abs dr > 1 && (getCharAtSpace' dungeonMap (r, c) ((quot dr 2), dc) == '*') =
    selfDestruction' (selfDestruction' dungeonMap (r, c)) ((r + (quot dr 2)), (c + dc))
  | abs dc > 1 && (getCharAtSpace' dungeonMap (r, c) (dr, (quot dc 2)) == '*') =
    selfDestruction' (selfDestruction' dungeonMap (r, c)) ((r + dr), (c + (quot dc 2)))
  | otherwise                                                                  =
    moveItem' dungeonMap (r, c) (dr, dc)

-- Determines the order of priority of spaces to attempt to move to, given the orientation of the
-- goal to the monster's current position. The base example is if the goal is SSE of the monster.
-- Give the actual orientation (and the list of spaces in priority order for SSE), this method
-- rearranges the spaces into actual priority order
monsterPriorityCheck' :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
monsterPriorityCheck' (r, c) base
  | r > 0 && c > 0 && abs r > abs c = base -- SSE
  | r > 0 && c > 0                  = map (\(x, y) -> (y, x)) base -- SEE
  | r > 0 && abs r > abs c          = map (\(x, y) -> (x, (-1 * y))) base -- SSW
  | r > 0                           = map (\(x, y) -> (y, (-1 * x))) base -- SWW
  | c > 0 && abs r > abs c          = map (\(x, y) -> ((-1 * x) , y)) base -- NNE
  | c > 0                           = map (\(x, y) -> ((-1 * y), x)) base -- NEE
  | abs r > abs c                   = map (\(x, y) -> ((-1 * x), (-1 * y))) base -- NNW
  | otherwise                       = map (\(x, y) -> ((-1 * y), (-1 * x))) base -- NWWs
