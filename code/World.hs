module World where
import Data.Array

data Direction = North | South | West | East
  deriving (Show,Eq,Read,Enum)

type Position = (Int, Int)
type Cell = [Direction]
type Maze = Array (Int, Int) Cell

move :: Direction -> Position -> Position
move North  (x, y) = (x, y+1)
move West   (x, y) = (x-1, y)
move South  (x, y) = (x, y-1)
move East   (x, y) = (x+1, y)

moves :: [Direction] -> Position -> Position 
moves = flip $ foldr move 

validMove :: Maze -> Position -> Position -> Bool
validMove m p1 p2 = undefined

fromList :: [(Position, [Direction])] -> Maze
fromList xs = undefined
