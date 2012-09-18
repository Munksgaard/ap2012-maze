module MEL where
import World

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

testMaze :: Maze
testMaze = fromList [((0,0),[North,South,West]),((0,1),[North,South,West])
                    ,((0,2),[South,West]),((0,3),[West,East])
                    ,((0,4),[North,West]),((1,0),[South]),((1,1),[North])
                    ,((1,2),[South,East]),((1,3),[North,West])
                    ,((1,4),[North,South,East]),((2,0),[North,South])
                    ,((2,1),[South,East]),((2,2),[West,East]),((2,3),[])
                    ,((2,4),[North,West,East]),((3,0),[North,South])
                    ,((3,1),[South,West]),((3,2),[West])
                    ,((3,3),[]),((3,4),[North,West,East])
                    ,((4,0),[North,South,East]),((4,1),[North,South,East])
                    ,((4,2),[North,South,East]),((4,3),[South,East])
                    ,((4,4),[North,West,East])]

data Robot = Robot  { position :: Position
                    , direction :: Direction
                    , history::[Position]
                    }

type World = (Robot, Maze)

newtype RobotCommand a = RC {runRC :: World -> (a, World)}

initialWorld :: Maze -> World
initialWorld m = (Robot{position = (0,0), direction = North, history=[]}, m)

instance Monad RobotCommand where
    return x = RC $ \s -> (x, s)
    (RC h) >>= f = RC $ \s -> let (a, newWorld) = h s
                                  (RC g) = f a  
                              in g newWorld

tryMove :: Maze -> Position -> Direction -> Position
tryMove maze pos dir = let newpos = move dir pos in
                         if validMove maze pos newpos then
                             newpos
                         else pos

evalC :: Cond -> Bool
evalC (Wall r)  = True
evalC (And c1 c2) = evalC c1 && evalC c2 
evalC (Not c)   = not $ evalC c

-- (World -> (a , World)) -> (a -> (World -> (b, World))) -> (World -> (b, World))
interp :: Stm -> RobotCommand ()
interp Forward = RC $ \(robot, maze) -> ((), (newRobot maze robot, maze))
    where newRobot maze robot = Robot (newPosition maze robot) (direction robot)
                                (newHistory robot)
          newPosition maze robot = tryMove maze (position robot) (direction robot)
          newHistory robot = position robot : history robot
interp Backward = RC $ \(robot, maze) -> ((), (newRobot maze robot, maze))
    where newRobot maze robot = Robot (newPosition maze robot) (direction robot)
                                (newHistory robot)
          newPosition maze robot = tryMove maze (position robot) (otherDir $ direction robot)
          newHistory robot = position robot : history robot
interp TurnRight = RC $ \(robot, maze) -> ((), (newRobot robot, maze))
    where newRobot robot = Robot (position robot) (rightTurn $ direction robot)
                                (history robot)
interp TurnLeft = RC $ \(robot, maze) -> ((), (newRobot robot, maze))
    where newRobot robot = Robot (position robot) (leftTurn $ direction robot)
                                (history robot)
interp (If _ _ _) = undefined
interp (While _ _) = undefined
interp (Block []) = return ()
interp (Block (stm:stms)) = do 
  interp stm
  interp $ Block stms

-- Fx. (runRC (interp Forward)) ini
-- testIt = do
--   interp Forward
--   interp Forward

runProg :: Maze -> Program -> ([Position], Direction)
runProg maze prog = let (_, (robot, _)) = runRC (interp prog) (initialWorld maze)
                    in (reverse (position robot : history robot), direction robot)

-- runProg testMaze $ Block [TurnRight, Forward]
