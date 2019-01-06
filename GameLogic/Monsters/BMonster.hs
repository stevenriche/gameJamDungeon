module GameLogic.Monsters.BMonster
( advanceOneBMonster'
) where

import GameLogic.PlayerMovement
import GameLogic.Util
import GameLogic.Monsters.MonsterUtil

-- Specifically moves the 'B' type monster
-- The B monster tries to run directly at the exit and block it
-- A priority check is done to find what spaces it wants to go to most, then it finds the first
-- unblocked space and goes there.
advanceOneBMonster' :: [[Char]] -> (Int, Int) -> [[Char]]
advanceOneBMonster' dungeonMap (r, c)
  | (abs dR <= 1 && abs dC <= 1) = moveItem' dungeonMap (r, c) (dR, dC)
  | (length openSpaces > 0)      = moveIfNotMined' dungeonMap (r, c) (openSpaces!!0)
  | otherwise                    = dungeonMap
  where (xR, xC) = characterLocation' dungeonMap 'x'
        (dR, dC) = ((xR - r), (xC - c))
        spacesPriorityOrder = monsterPriorityCheck' (dR, dC) bPriorityOrder
        openSpaces = filter (\sp -> isSpaceOpen' dungeonMap (r, c) sp) spacesPriorityOrder

-- If the exit is SSE of the monster, the following is the priority list
-- of spaces the monster will attempt to go
--             __
--          __| 4|__
--         | 3| M| 2|
--            | 1|
--
-- If the exit is some other direction from the monster, the monsterPriorityCheck method will flip
-- and reorient the priority order as needed
bPriorityOrder = [(1, 0), (0, 1), (0, -1), (-1, 0)]
