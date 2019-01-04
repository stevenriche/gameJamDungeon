import GameLogic.GameLoop
import Levels.LevelsList

-- Main input/output function
main = gameloop' ("CONTINUE", ("Welcome to the dungeon!" : (levels !! 0))) 0
