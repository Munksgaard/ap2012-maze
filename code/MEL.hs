module MEL where
import World as W

data Relative = Ahead | ToLeft | ToRight | Behind
  deriving (Eq, Show)

data Cond = Wall Relative
          | And Cond Cond
          | Not Cond
          deriving (Eq, Show)

data Stm = Forward
         | Backward
         | TurnRight
         | TurnLeft
         | If Cond Stm Stm
         | While Cond Stm
         | Block [Stm]
         deriving (Eq, Show)

type Program = Stm

-- Definition of type is only to make it typecheck
type World = (String, Maze)

--newtype RobotCommand a = RC {runRC :: World -> ... }

initialWorld :: Maze -> World
initialWorld m = undefined

-- interp :: Stm -> RobotCommand()

runProg :: Maze -> Program -> ([Position], Direction)
runProg m p = undefined
