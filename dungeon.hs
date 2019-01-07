import GameLogic.GameLoop
import Levels.LevelsList

-- Main input/output function
main = gameloop' ("WIN", ("" : (levels !! 0))) 0
