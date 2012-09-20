module Main where
import World
import MEL as M
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
prompt :: Stm -> IO ()
prompt program@(Block cs) = do
            print "Your robot is currently at"
            print $ runProg testMaze program
            print "What is your command? "
            cmd <- getLine
            case cmd of
              ":q" -> print "Thank you, please come again."
              command -> do
                          print command
                          let c = read command in prompt $ Block (cs ++ [c])
                          return $ ()
main :: IO ()
main = prompt $ Block []
