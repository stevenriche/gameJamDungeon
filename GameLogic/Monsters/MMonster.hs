module GameLogic.Monsters.MMonster
( advanceOneMMonster'
) where

import GameLogic.PlayerMovement
import GameLogic.Util
import GameLogic.Monsters.MonsterUtil

-- Specifically moves the 'M' type monster
-- The M monster tries to run directly at the player's position
-- A priority check is done to find what spaces it wants to go to most, then it finds the first
-- unblocked space and goes there.
advanceOneMMonster' :: [[Char]] -> (Int, Int) -> [[Char]]
advanceOneMMonster' dungeonMap (r, c)
  | (abs dR <= 1 && abs dC <= 1) = moveItem' dungeonMap (r, c) (dR, dC)
  | (length openSpaces > 0)      = moveIfNotMined' dungeonMap (r, c) (openSpaces!!0)
  | otherwise                    = dungeonMap
  where (plR, plC) = playerLocation' dungeonMap
        (dR, dC) = ((plR - r), (plC - c))
        spacesPriorityOrder = monsterPriorityCheck' (dR, dC) mPriorityOrder
        openSpaces = filter (\sp -> mMonsterBlockCheck' dungeonMap (r, c) sp) spacesPriorityOrder

-- If the player is SSE of the monster, the following is the priority list
-- of spaces the monster will attempt to go (the monster can move 2 spaces at once)
--                __
--             __|12|__
--          __|11| 9| 7|__
--         |10| 8| M| 5| 3|
--            | 6| 4| 2|
--               | 1|
-- If the player is some other direction from the monster, monsterPriorityCheck method will flip and
-- reorient the priority order as needed
mPriorityOrder = [(2, 0), (1, 1), (0, 2), (1, 0), (0, 1), (1, -1), (-1, 1), (0, -1), (-1, 0), (0, -2), (-1, -1), (-2, 0)]

-- This method checks if the path is blocked for an 'M' monster. Since this monster can move two
-- spaces at once, we have to do a check for both the space it lands and the space in between
mMonsterBlockCheck' :: [[Char]] -> (Int, Int) -> (Int, Int) -> Bool
mMonsterBlockCheck' dungeonMap coords (r, c)
  | abs r < 2 && abs c < 2 = isSpaceOpen' dungeonMap coords (r, c)
  | abs r < 2              = (isSpaceOpen' dungeonMap coords (r, (quot c 2))) && (isSpaceOpen' dungeonMap coords (r, c))
  | otherwise              = (isSpaceOpen' dungeonMap coords ((quot r 2), c)) && (isSpaceOpen' dungeonMap coords (r, c))
