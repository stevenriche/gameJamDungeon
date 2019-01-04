module GameLogic.GameLoop
( gameloop'
) where

import System.IO
import GameLogic.PlayerMovement
import Levels.LevelsList

-- Gameplay loop that continues until it is flagged that the game is over
gameloop' :: ([Char], [[Char]]) -> Int -> IO [()]
gameloop' (status, (message:dungeonMap)) levelIndex
  | status == "END" = renderScreen' message dungeonMap
  | status == "WIN" = nextLevel' message dungeonMap levelIndex
  | otherwise       = continueGame' message dungeonMap levelIndex

-- Move to the next level or beat the game
nextLevel' :: [Char] -> [[Char]] -> Int -> IO [()]
nextLevel' message dungeonMap levelIndex
  | length levels <= (levelIndex + 1) = gameloop' ("END", ("You beat the game!" : dungeonMap)) levelIndex
  | otherwise                         = do
    renderScreen' message dungeonMap
    _ <- getLine
    gameloop' ("CONTINUE", ("Next Level!" : (levels !! (levelIndex + 1)))) (levelIndex + 1)

-- Continue in gameplay loop, and query the player for next move
continueGame' :: [Char] -> [[Char]] -> Int -> IO [()]
continueGame' message dungeonMap levelIndex = do
  renderScreen' message dungeonMap
  queryDirection' dungeonMap levelIndex

-- Prepare screen, print message and map
renderScreen' :: [Char] -> [[Char]] -> IO [()]
renderScreen' message dungeonMap = do
  hPutStr stderr $ "\r\ESC[1J" -- clears screen
  hPutStr stderr $ "\r\ESC[22A" -- puts cursor 22 lines above
  putStrLn message
  printDungeon' dungeonMap

-- Print out our ASCII dungeon
printDungeon' :: [[Char]] -> IO [()]
printDungeon' dungeonMap = sequence (map putStrLn dungeonMap)

-- take character input from user, probably direction
queryDirection' :: [[Char]] -> Int -> IO [()]
queryDirection' dungeonMap levelIndex = do
  direction <- getLine
  gameloop' (takeInput' direction dungeonMap) levelIndex

-- user input - if valid direction, returns the updated dungeon map
takeInput' :: [Char] -> [[Char]] -> ([Char], [[Char]])
takeInput' direction dungeonMap
  | direction `elem` ["w", "a", "s", "d"]   = move' direction dungeonMap
  | direction == "q"                        = ("END", ("Thanks for playing" : dungeonMap))
  | otherwise                               = ("CONTINUE", ("That is not a valid direction!" : dungeonMap))
