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

--newtype State a = S (State a -> (a, State a))

--instance Monad State where
--  return x = undefined
--  S f >>= g = undefined

data Robot = Robot  { position :: Position
                    , direction :: Direction
                    , history::[Position]
                    }

type World = (Robot, Maze)

newtype RobotCommand a = RC {runRC :: World -> Robot -> World }

initialWorld :: Maze -> World
initialWorld m = (Robot{position = (0,0), direction = North, history=[]}, m)

fromRelative :: Relative -> Direction -> Direction

fromRelative Ahead d    = d
fromRelative ToLeft d   = pred d
fromRelative ToRight d  = succ d
fromRelative Behind d   = succ $ succ d

evalC :: Cond -> Bool
evalC (Wall r)  = True
evalC (And c1 c2) = evalC c1 && evalC c2 
evalC (Not c)   = not $ evalC c

interp :: Stm -> RobotCommand()
interp Forward      = undefined
interp Backward     = undefined
interp TurnRight    = undefined
interp TurnLeft     = undefined
interp (If c s0 s1) = if evalC c then interp s0 else interp s1
interp (While c s)  = undefined
interp (Block [])   = undefined
interp (Block [s]) = undefined

runProg :: Maze -> Program -> ([Position], Direction)
runProg m p = undefined
