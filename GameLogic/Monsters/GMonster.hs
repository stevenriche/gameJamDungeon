module GameLogic.Monsters.GMonster
( advanceOneGMonster'
) where

import GameLogic.PlayerMovement
import GameLogic.Util
import GameLogic.Monsters.MonsterUtil

-- Specifically moves the 'G' type monster
-- The G monster tries to run directly at the player's position
-- The G monster can only move one space at a time, but can move through barriers
-- (leaving gaps the player can move through).
-- A priority check is done to find what spaces it wants to go to most, then it finds the first
-- unblocked space and goes there.
advanceOneGMonster' :: [[Char]] -> (Int, Int) -> [[Char]]
advanceOneGMonster' dungeonMap (r, c)
  | (abs dR <= 1 && abs dC <= 1) = moveItem' dungeonMap (r, c) (dR, dC)
  | otherwise                    = moveIfNotMined' dungeonMap (r, c) (spacesPriorityOrder!!0)
  where (plR, plC) = playerLocation' dungeonMap
        (dR, dC) = ((plR - r), (plC - c))
        spacesPriorityOrder = monsterPriorityCheck' (dR, dC) gPriorityOrder

-- If the player is SSE of the monster, the following is the priority list
-- of spaces the monster will attempt to go
--             __
--          __| 4|__
--         | 3| M| 2|
--            | 1|
--
-- If the player is some other direction from the monster, the monsterPriorityCheck method will flip
-- and reorient the priority order as needed
gPriorityOrder = [(1, 0), (0, 1), (0, -1), (-1, 0)]
