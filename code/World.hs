module World (Direction(..), Position, Cell, Maze, fromList, move, moves, validMove, toDirection, withinBounds,) where
import Data.Array as A
import Data.Maybe

data Direction = North | East | South | West
  deriving (Show,Eq,Read,Enum)

type Position = (Int, Int)
--newtype Position a = Position{getPosition :: (a,a)}
--instance Monad Position where
--  return x = Position (x,x)
--  Position (x,y) >>= f = 

type Cell = [Direction]
type Maze = Array Position Cell

-- Produces an empty maze with the specified size, starts at (0,0)
emptyMaze :: Position -> Maze
emptyMaze (x,y) = A.array ((0,0), (x,y)) 
                  [((x1,y1), []) | x1 <- [0..x], y1 <- [0..y]]

-- Generate a new instance of maze with bounds as specified.
-- Check for walls are lazy
-- assert bound > (0,0)
generateMaze :: Position -> [(Position, Cell)] -> Maze
generateMaze (x,y) cells = 
    A.array ((0,0), (x,y)) 
         [((x1,y1), Data.Maybe.fromMaybe [] $ lookup (x1,y1) cells) 
              | x1 <- [0..x], y1 <- [0..y]]
 
-- Returns a position if the move is valid, else it returns nothing
move :: Direction -> Position -> Position
--move d (x,y) | validMove 
move North  (x, y) = (x, y+1)
move West   (x, y) = (x-1, y)
move South  (x, y) = (x, y-1)
move East   (x, y) = (x+1, y)

moves :: [Direction] -> Position -> Position 
moves = flip $ foldr move 

-- Calculates the distance on the board between two positions
-- largest size doesnt matter, however if they have different signs the maze is
-- invalid
dist :: Position -> Position -> Int
dist (x1,y1) (x2,y2) = x + y 
  where x = abs $ x1 - x2
        y = abs $ y1 - y2 

-- Assert withinBounds p1 & withinBounds p2 
-- Assert Pos > (0,0)
-- Assert dist p1 p2 == 1
toDirection :: Position -> Position -> Maybe Direction
toDirection (x1,y1) (x2,y2)  | x == 0 && y > 0 = Just North
                              | x == 0 && y < 0 = Just South
                              | x < 0 && y == 0 = Just West
                              | x > 0 && y == 0 = Just East
                              | otherwise = Nothing
                              where x = x2 - x1
                                    y = y2 - y1

-- Test wether the position is within bounds of the maze
-- Asserts Pos > (0,0)
withinBounds :: Maze -> Position -> Bool
withinBounds m = (>) (snd (A.bounds m))

-- Tests wether a move from one position to the other is valid on a given board
-- Assert withinBounds p1 & withinBounds p2
validMove :: Maze -> Position -> Position -> Bool
validMove m p1 p2 | dist p1 p2 == 1 = not $ or [blocked p1 p2, blocked p2 p1]
                    -- Check if the direction has a wall
                    -- if not then take the cell in that direction and see if it
                    -- has a wall facing towards us
                  | otherwise = False 
    where blocked from to = case (toDirection from to) of
                                Just dir -> dir `elem` m!from
                                Nothing -> False


--listBounds :: [(Position, Cell)]  -> (Position,Position) -> (Position,Position)
--listBounds [] p = p
--listBounds (((x1,y1),_):xs) ((x2, y2),(x3,y3)) = listBounds xs ((min x1 x2, min y1 y2), (max x1 x3, max y1 y3))

-- Produces a maze from the cells
-- Assert Forall (p,c) in xs: withinBounds p
fromList :: [(Position, Cell)] -> Maze
--fromList xs = A.array . (listBounds xs ((0,0),(9,9))) xs
fromList = A.array ((0,0),(9,9))
