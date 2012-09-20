module MEL where
import World
import Data.Array

data Relative = Ahead | ToLeft | ToRight | Behind
  deriving (Eq, Show)

data Cond = Wall Relative
          | And Cond Cond
          | Not Cond
          | AtGoalPos
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
           deriving (Show)

type World = (Robot, Maze)

data Result = Win ([Position], Direction)
            | Stall ([Position], Direction)
            | MoveError String Robot
              deriving (Show)

newtype RobotCommand a = RC {runRC :: World -> Either (String, Robot) (a, Robot)}

absDir :: Robot -> Relative -> Direction
absDir robot Ahead = direction robot
absDir robot ToLeft = leftTurn $ direction robot
absDir robot ToRight = rightTurn $ direction robot
absDir robot Behind = oppositeDir $ direction robot

initialWorld :: Maze -> World
initialWorld m = (Robot{position = (0,0), direction = North, history=[]}, m)

instance Monad RobotCommand where
    return x = RC $ \(rb, _) -> Right (x, rb)
    (RC h) >>= f = RC $ \(rb,mz) -> case h (rb, mz) of
                                      Left e -> Left e
                                      Right (a, rb1) -> let RC g = f a
                                        in g (rb1, mz)

tryMove :: Maze -> Position -> Direction -> Position
tryMove maze pos dir = let newpos = move dir pos in
                         if validMove maze pos newpos then
                             newpos
                         else pos

-- Moves the robot in a given direction, returns false if 
moveRobot :: Maze -> Robot -> Direction -> Maybe Position
moveRobot m r d = if validMove m oldPos newPos then Just newPos else Nothing
                where oldPos = position r
                      newPos = move d oldPos

evalC :: Maze -> Robot -> Cond -> Bool
evalC maze robot (Wall rel) = not $ validMove maze (position robot) $ move (absDir robot rel) (position robot)
evalC maze robot (And c1 c2) = evalC maze robot c1 && evalC maze robot c2
evalC maze robot (Not c) = not $ evalC maze robot c
evalC maze robot AtGoalPos = position robot == snd (bounds maze)

getWorld :: RobotCommand World
getWorld = RC $ \(r,m) -> Right ((r,m),r)

setRobot :: Robot -> RobotCommand ()
setRobot newRobot = RC $ \_ -> Right ((), newRobot)

robotError :: (String, Robot) -> RobotCommand ()
robotError e = RC $ \_ -> Left e


-- (World -> (a , World)) -> (a -> (World -> (b, World))) -> (World -> (b, World))
interp :: Stm -> RobotCommand ()
interp Forward = do 
  (robot, maze) <- getWorld
  let d = direction robot 
  case moveRobot maze robot d of 
    Just p -> setRobot Robot { position=p
                             , direction=d
                             , history=position robot : history robot}
    Nothing -> robotError ( "Could not move " ++ show d ++ " in position " ++ show (position robot), robot)
interp Backward = do
  (robot, maze) <- getWorld
  let d = oppositeDir $ direction robot
  case moveRobot maze robot d of
    Just p -> setRobot Robot { position=p
                               , direction=d
                               , history= p:history robot}
    Nothing -> robotError ("Could not move " ++ show  d ++" in position " ++ show (position robot), robot)
interp TurnLeft = do
  (robot, _) <- getWorld
  let d = leftTurn $ direction robot
  setRobot Robot { position=position robot
                 , direction=d
                 , history= history robot}
interp TurnRight = do
  (robot, _) <- getWorld
  let d = rightTurn $ direction robot
  setRobot Robot { position=position robot
                 , direction=d
                 , history=history robot}
interp (If c s0 s1) = do
  (robot, maze) <- getWorld
  if evalC maze robot c then interp s0 else interp s1
interp (While c stm) = do
  (robot, maze) <- getWorld
  if evalC maze robot c then do
      interp stm
      interp $ While c stm
    else setRobot robot
interp (Block []) = return ()
interp (Block (stm:stms)) = do 
  interp stm
  interp $ Block stms

-- Fx. (runRC (interp Forward)) ini
-- testIt = do
--   interp Forward
--   interp Forward

runProg :: Maze -> Program -> Result
runProg maze prog = case runRC (interp prog) (initialWorld maze) of
                      Left (msg, robot) -> MoveError msg robot
                      Right (_, robot) ->
                          let p = position robot
                              h = reverse $ p : history robot
                              d = direction robot
                          in if p == snd (bounds maze) then Win (h,d)
                             else Stall (h,d)
-- runProg testMaze $ Block [TurnRight, Forward]
