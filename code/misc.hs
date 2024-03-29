import MEL
import World

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

-- test2 = interp TurnRight >>= (\x -> interp Forward >>= (\_ -> interp Forward))

win :: Result
win =  runProg testMaze $ Block [While (Not AtGoalPos) (If (Not $ Wall ToRight) (Block [TurnRight, Forward]) (If (Not $ Wall Ahead) Forward TurnLeft))]

-- ini = initialWorld testMaze

--bum = (runRC $ getWorld >>= (\_ -> interp TurnRight >>= (\x -> interp Forward >>= (\_ -> setRobot $ Robot{position  = (0,0), direction = North, history=[]})))) ini