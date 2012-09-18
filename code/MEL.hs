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

data Robot = Robot  { position :: Position
                    , direction :: Direction
                    , history::[Position]
                    }


type World = (Robot, Maze)

newtype RobotCommand a  = RC {runRC :: World -> (a, World)}

instance Monad RobotCommand where
  return x = RC (\s -> (x, s))
  RC st >>= f = RC (\s -> let (a, s') = st s in runRC (f a) s')


--  st >>= stm = RC (\s -> let (x, s') = runRC st s in (interp stm s' , s'))
--  s0 >>= f = RC (\s -> let (x,s') = apply s0 s in (apply (f x) s', s'))
  -- m a -> (a -> m b) -> m b
  -- (State a) -> (a -> State b) -> State b
  -- (s -> (a ,s)) -> (a -> s -> (b, s)) -> (s -> (b,s))
--  st >>= stm = RC (\s -> let (x, s') = st s in interp stm s')
--  st >>= f = RC (\s ->  let (x, s') = s in (f x, s'))
--  st >> f = RC {runRC = (

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

interp :: Stm -> RobotCommand ()
interp Forward      = return ()                              --(, North), t))--RC (\s -> s >>= (\(p,d) -> uncurry (\t -> move)))
                         --let ((p,d),s') = s in s >>= (move p d)) --(\((p,d),s) -> (move p d,s))  -- kig ind i robot, hente position, lave move (hvis valid), returner state
--interp Backward     = undefined
--interp TurnRight    = undefined
--interp TurnLeft     = undefined
interp (If c s0 s1) = if evalC c then interp s0 else interp s1
--interp (While c s)  = if evalC c then s:(While c s):[] else skip
interp (Block [])   = undefined
--interp (Block (x:xs)) = return x >>= interp 
--for each x
runProg :: Maze -> Program -> ([Position], Direction)
runProg m p = undefined
