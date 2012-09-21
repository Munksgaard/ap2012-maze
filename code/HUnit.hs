module HUnitTest where
import World
import MEL
import Test.HUnit
import Data.Array

positiveValidIndices :: [(Integer, Integer)]
positiveValidIndices = [ (0,0),(0,1),(0,2),(0,3),(0,4),(1,0),(1,1),(1,2),(1,3),(1,4)
                       , (2,0),(2,1),(2,2),(2,3),(2,4),(3,0),(3,1),(3,2),(3,3),(3,4)
                       , (4,0),(4,1),(4,2),(4,3),(4,4)]

positiveValidCells :: [[Direction]]
positiveValidCells = [[North,South,West],[North,South,West]
                    ,[South,West],[West,East]
                    ,[North,West],[South],[North]
                    ,[South,East],[North,West]
                    ,[North,South,East],[North,South]
                    ,[South,East],[West,East],[]
                    ,[North,West,East],[North,South]
                    ,[South,West],[West]
                    ,[],[North,West,East]
                    ,[North,South,East],[North,South,East]
                    ,[North,South,East],[South,East]
                    ,[North,West,East]]

positiveMaze :: Maze
positiveMaze = fromList positiveCells

negativeCells :: [((Int, Int), [Direction])]
negativeCells = [((0,0),[North,South,West]),((0,-1),[North,South,West])
                    ,((0,-2),[South,West]),((0,-3),[West,East])
                    ,((0,-4),[North,West]),((-1,0),[South]),((-1,-1),[North])
                    ,((-1,-2),[South,East]),((-1,-3),[North,West])
                    ,((-1,-4),[North,South,East]),((-2,-0),[North,South])
                    ,((-2,-1),[South,East]),((-2,-2),[West,East]),((-2,-3),[])
                    ,((-2,-4),[North,West,East]),((-3,-0),[North,South])
                    ,((-3,-1),[South,West]),((-3,-2),[West])
                    ,((-3,-3),[]),((-3,-4),[North,West,East])
                    ,((-4,-0),[North,South,East]),((-4,-1),[North,South,East])
                    ,((-4,-2),[North,South,East]),((-4,-3),[South,East])
                    ,((-4,-4),[North,West,East])]

negativeValidIndices :: [(Integer, Integer)]
negativeValidIndices = [(0,0)]

negativeValidCells :: [[Direction]]
negativeValidCells = [[North,South,West]]

negativeMaze :: Maze
negativeMaze = fromList negativeCells

allWallsMaze :: Maze
allWallsMaze = fromList [((0,0), [North, East, South, West])]

emptyMaze :: Maze
emptyMaze = fromList []

mixedCells :: [((Int, Int), [Direction])]
mixedCells = positiveCells ++ negativeCells

mixedValidCells :: [[Direction]]
mixedValidCells = positiveValidCells

mixedMaze :: Maze
mixedMaze = fromList mixedCells

-- Tests for validMove 
testValidMove1 :: Test
testValidMove1 = TestCase (assertEqual "validMove (0,0) (0,0)" False (validMove positiveMaze (0,0) (0,0)))

testValidMove2 :: Test
testValidMove2 = TestCase (assertEqual "validMove (4,4) (4,4)" False (validMove positiveMaze (4,4) (4,4)))

testValidMove3 :: Test
testValidMove3 = TestCase (assertEqual "validMove (2,3) (2,3)" False (validMove positiveMaze (2,3) (2,3)))

testValidMove4 :: Test
testValidMove4 = TestCase (assertEqual "validMove (0,0) (1,0)" True (validMove positiveMaze (0,0) (1,0)))

testValidMove5 :: Test
testValidMove5 = TestCase (assertEqual "validMove (0,0) (0,1)" False (validMove positiveMaze (0,0) (0,1)))

testValidMove6 :: Test
testValidMove6 = TestCase (assertEqual "validMove (0,0) (0,-1)" False (validMove positiveMaze (0,0) (0,-1)))

testValidMove7 :: Test
testValidMove7 = TestCase (assertEqual "validMove (0,0) (-1, 0)" False (validMove positiveMaze (0,0) (-1, 0)))

testValidMove8 :: Test
testValidMove8 = TestCase (assertEqual "validMove (-1, 0) (0,0)" False (validMove positiveMaze (-1, 0) (0,0)))

testValidMove9 :: Test
testValidMove9 = TestCase (assertEqual "validMove (0, -1) (0,0)" False (validMove positiveMaze (0, -1) (0,0)))

testValidMove10 :: Test
testValidMove10 = TestCase (assertEqual "validMove (-2, -3) (-2, -4)" False (validMove positiveMaze (-2, -3) (-2, -4)))

testValidMove11 :: Test
testValidMove11 = TestCase (assertEqual "validMove (-3, -2) (-4, -2)" False (validMove positiveMaze (-3, -2) (-4, -2)))

testValidMove12 :: Test
testValidMove12 = TestCase (assertEqual "validMove (-5, -5) (-5, -4)" False (validMove positiveMaze (-5, -5) (-5, -4)))

testValidMove13 :: Test
testValidMove13 = TestCase (assertEqual "validMove (-5, -5) (-4, -5)" False (validMove positiveMaze (-5, -5) (-4, -5)))

testValidMove14 :: Test
testValidMove14 = TestCase (assertEqual "validMove (1,0) (2,1)" False (validMove positiveMaze (1,0) (2,1)))

testValidMove15 :: Test
testValidMove15 = TestCase (assertEqual "validMove (2,1) (1,0)" False (validMove positiveMaze (2,1) (1,0)))

testValidMove16 :: Test
testValidMove16 = TestCase (assertEqual "validMove (2,0) (1,1)" False (validMove positiveMaze (2,0) (1,1)))

testValidMove17 :: Test
testValidMove17 = TestCase (assertEqual "validMove (1,1) (2,0)" False (validMove positiveMaze (1,1) (2,0)))

testValidMove18 :: Test
testValidMove18 = TestCase (assertEqual "validMove (-2, 2) (-2, 3)" False (validMove positiveMaze (-2, 2) (-2, 3)))

testValidMove19 :: Test
testValidMove19 = TestCase (assertEqual "validMove (-2, 3) (-2, 2)" False (validMove positiveMaze (-2, 3) (-2, 2)))

testValidMove20 :: Test
testValidMove20 = TestCase (assertEqual "validMove (-2, 3) (-1, 3)" False (validMove positiveMaze (-2, 3) (-1, 3)))

testValidMove21 :: Test
testValidMove21 = TestCase (assertEqual "validMove (-1, 3) (-2, 3)" False (validMove positiveMaze (-1, 3) (-2, 3)))

testValidMove22 :: Test
testValidMove22 = TestCase (assertEqual "validMove (-2, 3) (-1, 2)" False (validMove positiveMaze (-2, 3) (-1, 2)))

testValidMove23 :: Test
testValidMove23 = TestCase (assertEqual "validMove (-2, 3) (-3, 4)" False (validMove positiveMaze (-2, 3) (-3, 4)))

testValidMove24 :: Test
testValidMove24 = TestCase (assertEqual "validMove (-2, 3) (-3, 4)" False (validMove positiveMaze (-2, 3) (-3, 4)))

testValidMove25 :: Test
testValidMove25 = TestCase (assertEqual "validMove (-3, 4) (-2, 3)" False (validMove positiveMaze (-3, 4) (-2, 3)))

testValidMove26 :: Test
testValidMove26 = TestCase (assertEqual "validMove (2, -2) (2, -3)" False (validMove positiveMaze (2, -2) (2, -3)))

testValidMove27 :: Test
testValidMove27 = TestCase (assertEqual "validMove (2, -3) (2, -2)" False (validMove positiveMaze (2, -3) (2, -2)))

testValidMove28 :: Test
testValidMove28 = TestCase (assertEqual "validMove (2, -2) (3, -2)" False (validMove positiveMaze (2, -2) (3, -2)))

testValidMove29 :: Test
testValidMove29 = TestCase (assertEqual "validMove (3, -2) (2, -2)" False (validMove positiveMaze (3, -2) (2, -2)))

testValidMove30 :: Test
testValidMove30 = TestCase (assertEqual "validMove (2, -2) (3, -1)" False (validMove positiveMaze (2, -2) (3, -1)))

testValidMove31 :: Test
testValidMove31 = TestCase (assertEqual "validMove (3, -1) (2, -2)" False (validMove positiveMaze (3, -1) (2, -2)))

testValidMove32 :: Test
testValidMove32 = TestCase (assertEqual "validMove (2, -2) (3, -3)" False (validMove positiveMaze (2, -2) (3, -3)))

testValidMove33 :: Test
testValidMove33 = TestCase (assertEqual "validMove (0, 0) (4, 0)" False (validMove positiveMaze (0, 0) (4, 0)))

testValidMove34 :: Test
testValidMove34 = TestCase (assertEqual "validMove (4, 0) (0, 0)" False (validMove positiveMaze (4, 0) (0, 0)))

testValidMove35 :: Test
testValidMove35 = TestCase (assertEqual "validMove (2, 1) (2, 4)" False (validMove positiveMaze (2, 1) (2, 4)))

testValidMove36 :: Test
testValidMove36 = TestCase (assertEqual "validMove (4, 2) (2, 1)" False (validMove positiveMaze (4,2) (2, 1)))

testValidMove37 :: Test
testValidMove37 = TestCase (assertEqual "validMove (6,6) (7,7)" False (validMove positiveMaze (6,6) (7,7)))

testValidMove38 :: Test
testValidMove38 = TestCase (assertEqual "validMove (7,7) (6,6)" False (validMove positiveMaze (7,7) (6,6)))

testValidMove39 :: Test
testValidMove39 = TestCase (assertEqual "validMove (8,8) (7,8)" False (validMove positiveMaze (8,8) (7,8)))

testValidMove40 :: Test
testValidMove40 = TestCase (assertEqual "validMove (7,8) (8,8)" False (validMove positiveMaze (7,8) (8,8)))

testValidMove41 :: Test
testValidMove41 = TestCase (assertEqual "validMove (8,8) (8,7)" False (validMove positiveMaze (8,8) (8,7)))

testValidMove42 :: Test
testValidMove42 = TestCase (assertEqual "validMove (0,4) (0,5)" False (validMove positiveMaze (0,4) (0,5)))

testValidMove43 :: Test
testValidMove43 = TestCase (assertEqual "validMove (0,4) (-1,4)" False (validMove positiveMaze (0,4) (-1,4)))

testValidMove44 :: Test
testValidMove44 = TestCase (assertEqual "validMove (4,0) (5,0)" False (validMove positiveMaze (4,0) (5,0)))

testValidMove45 :: Test
testValidMove45 = TestCase (assertEqual "validMove (4,0) (4,-1)" False (validMove positiveMaze (4,0) (4,-1)))

testValidMove46 :: Test
testValidMove46 = TestCase (assertEqual "validMove (4,4) (4,5)" False (validMove positiveMaze (4,4) (4,5)))

testValidMove47 :: Test
testValidMove47 = TestCase (assertEqual "validMove (4,4) (5,4)" False (validMove positiveMaze (4,4) (5,4)))

validMoveTestList :: Test
validMoveTestList = TestList[TestLabel "testValidMove1" testValidMove1
                            ,TestLabel "testValidMove2" testValidMove2
                            ,TestLabel "testValidMove3" testValidMove3
                            ,TestLabel "testValidMove4" testValidMove4
                            ,TestLabel "testValidMove5" testValidMove5
                            ,TestLabel "testValidMove6" testValidMove6
                            ,TestLabel "testValidMove7" testValidMove7
                            ,TestLabel "testValidMove8" testValidMove8
                            ,TestLabel "testValidMove9" testValidMove9
                            ,TestLabel "testValidMove10" testValidMove10
                            ,TestLabel "testValidMove11" testValidMove11
                            ,TestLabel "testValidMove12" testValidMove12
                            ,TestLabel "testValidMove13" testValidMove13
                            ,TestLabel "testValidMove14" testValidMove14
                            ,TestLabel "testValidMove15" testValidMove15
                            ,TestLabel "testValidMove16" testValidMove16
                            ,TestLabel "testValidMove17" testValidMove17
                            ,TestLabel "testValidMove18" testValidMove18
                            ,TestLabel "testValidMove19" testValidMove19
                            ,TestLabel "testValidMove20" testValidMove20
                            ,TestLabel "testValidMove21" testValidMove21
                            ,TestLabel "testValidMove22" testValidMove22
                            ,TestLabel "testValidMove23" testValidMove23
                            ,TestLabel "testValidMove24" testValidMove24
                            ,TestLabel "testValidMove25" testValidMove25
                            ,TestLabel "testValidMove26" testValidMove26
                            ,TestLabel "testValidMove27" testValidMove27
                            ,TestLabel "testValidMove28" testValidMove28
                            ,TestLabel "testValidMove29" testValidMove29
                            ,TestLabel "testValidMove30" testValidMove30
                            ,TestLabel "testValidMove31" testValidMove31
                            ,TestLabel "testValidMove32" testValidMove32
                            ,TestLabel "testValidMove33" testValidMove33
                            ,TestLabel "testValidMove34" testValidMove34
                            ,TestLabel "testValidMove35" testValidMove35
                            ,TestLabel "testValidMove36" testValidMove36
                            ,TestLabel "testValidMove37" testValidMove37
                            ,TestLabel "testValidMove38" testValidMove38
                            ,TestLabel "testValidMove39" testValidMove39
                            ,TestLabel "testValidMove40" testValidMove40
                            ,TestLabel "testValidMove41" testValidMove41
                            ,TestLabel "testValidMove42" testValidMove42
                            ,TestLabel "testValidMove43" testValidMove43
                            ,TestLabel "testValidMove44" testValidMove44
                            ,TestLabel "testValidMove45" testValidMove45
                            ,TestLabel "testValidMove46" testValidMove46
                            ,TestLabel "testValidMove47" testValidMove47]

-- Test that oppositeDir truly returns the opposite dir
oppositeDirTest1 :: Test
oppositeDirTest1 = TestCase $ assertEqual "oppositeDir in: North" South (oppositeDir North)

oppositeDirTest2 :: Test
oppositeDirTest2 = TestCase $ assertEqual "oppositeDir in: East" West (oppositeDir East)

oppositeDirTest3 :: Test
oppositeDirTest3 = TestCase $ assertEqual "oppositeDir in: South" North (oppositeDir South)

oppositeDirTest4 :: Test
oppositeDirTest4 = TestCase $ assertEqual "oppositeDir in: West" East (oppositeDir West)
-- No need for further testing here since this is all the possible arguments and
-- the function is deterministic
 
oppositeDirTestList :: Test
oppositeDirTestList = TestList [ TestLabel "oppositeDirTest1" oppositeDirTest1
                               , TestLabel "oppositeDirTest2" oppositeDirTest2
                               , TestLabel "oppositeDirTest3" oppositeDirTest3
                               , TestLabel "oppositeDirTest4" oppositeDirTest4
                               ]

-- Test that the function truly makes a right turn
rightTurnTest1 :: Test
rightTurnTest1 = TestCase $ assertEqual "rightTurn in: North" East (rightTurn North)

rightTurnTest2 :: Test
rightTurnTest2 = TestCase $ assertEqual "rightTurn in: East" South (rightTurn East)

rightTurnTest3 :: Test
rightTurnTest3 = TestCase $ assertEqual "rightTurn in: South" West (rightTurn South)

rightTurnTest4 :: Test
rightTurnTest4 = TestCase $ assertEqual "rightTurn in: West" North (rightTurn West)

-- Test that the function wraps after 4 rightturns
rightTurnTest5 :: Test
rightTurnTest5 = TestCase $ assertEqual "rightTurn wrap" North 
  (rightTurn $ rightTurn $ rightTurn $ rightTurn North)

rightTurnTestList :: Test
rightTurnTestList = TestList [ TestLabel "rightTurnTest1" rightTurnTest1
                             , TestLabel "rightTurnTest2" rightTurnTest2
                             , TestLabel "rightTurnTest3" rightTurnTest3
                             , TestLabel "rightTurnTest4" rightTurnTest4
                             , TestLabel "rightTurnTest5" rightTurnTest5
                             ]

 -- Test that the function truly makes a left turn
leftTurnTest1 :: Test
leftTurnTest1 = TestCase $ assertEqual "leftTurn in: North" West (leftTurn North)

leftTurnTest2 :: Test
leftTurnTest2 = TestCase $ assertEqual "leftTurn in: East" North (leftTurn East)

leftTurnTest3 :: Test
leftTurnTest3 = TestCase $ assertEqual "leftTurn in: South" East (leftTurn South)

leftTurnTest4 :: Test
leftTurnTest4 = TestCase $ assertEqual "leftTurn in: West" South (leftTurn West)
-- All possible values

-- Test that the function wraps after 4 leftturns
leftTurnTest5 :: Test
leftTurnTest5 = TestCase $ assertEqual "leftTurn wrap" North 
  (leftTurn $ leftTurn $ leftTurn $ leftTurn North)

leftTurnTestList :: Test
leftTurnTestList = TestList [ TestLabel "leftTurnTest1" leftTurnTest1
                             , TestLabel "leftTurnTest2" leftTurnTest2
                             , TestLabel "leftTurnTest3" leftTurnTest3
                             , TestLabel "leftTurnTest4" leftTurnTest4
                             , TestLabel "leftTurnTest5" leftTurnTest5
                             ]

-- Test that right- and leftTurns are truly opposite
rightleftTest1 :: Test
rightleftTest1 = TestCase $ assertEqual "right/left Test1 in: North" 
  (rightTurn North) (oppositeDir (leftTurn North))

rightleftTest2 :: Test
rightleftTest2 = TestCase $ assertEqual "right/left Test2 in: East" 
  (rightTurn East) (oppositeDir (leftTurn East))

rightleftTest3 :: Test
rightleftTest3 = TestCase $ assertEqual "right/left Test3 in: South" 
  (rightTurn South) (oppositeDir (leftTurn South))

rightleftTest4 :: Test
rightleftTest4 = TestCase $ assertEqual "right/left Test4 in: West" 
  (rightTurn West) (oppositeDir (leftTurn West))

rightleftTestList :: Test
rightleftTestList = TestList [ TestLabel "right/left test1" rightleftTest1
                    , TestLabel "right/left test2" rightleftTest2
                    , TestLabel "right/left test3" rightleftTest3
                    , TestLabel "right/left test4" rightleftTest4
                    ]

-- The move function asserts that the position and direction is a validmove.
-- Test move in all directions
moveTest1 :: Test
moveTest1 = TestCase $ assertEqual "move test1 in: West (1,0)" (0,0) (move West (1,0))

moveTest2 :: Test
moveTest2 = TestCase $ assertEqual "move test1 in: North (1,0)" (1,1) (move North (1,0))

moveTest3 :: Test
moveTest3 = TestCase $ assertEqual "move test1 in: East (1,0)" (2,0) (move East (1,0))

moveTest4 :: Test
moveTest4 = TestCase $ assertEqual "move test1 in: North (0,1)" (0,2) (move North (0,1))

moveTest5 :: Test
moveTest5 = TestCase $ assertEqual "move test1 in: East (0,1)" (1,1) (move East (0,1))

moveTest6 :: Test
moveTest6 = TestCase $ assertEqual "move test1 in: South (0,1)" (0,0) (move South (0,1))

moveTest7 :: Test
moveTest7 = TestCase $ assertEqual "move test1 in: North (1,1)" (1,2) (move North (1,1))

moveTest8 :: Test
moveTest8 = TestCase $ assertEqual "move test1 in: East (1,1)" (2,1) (move East (1,1))

moveTest9 :: Test
moveTest9 = TestCase $ assertEqual "move test1 in: South (1,1)" (1,0) (move South (1,1))

moveTest10 :: Test
moveTest10 = TestCase $ assertEqual "move test1 in: West (1,1)" (0,1) (move West (1,1))

moveTestList :: Test
moveTestList = TestList [ TestLabel "move test1" moveTest1
                        , TestLabel "move test2" moveTest2
                        , TestLabel "move test3" moveTest3
                        , TestLabel "move test4" moveTest4
                        , TestLabel "move test5" moveTest5
                        , TestLabel "move test6" moveTest6
                        , TestLabel "move test7" moveTest7
                        , TestLabel "move test8" moveTest8
                        , TestLabel "move test9" moveTest9
                        , TestLabel "move test10" moveTest10
                        ]

-- Test each direction horizontal, vertical and diagonal (while we're at it)
--distTest :: Position -> Position -> Test
--distTest p1 p2 res = TestCase $ assertEqual ("dist test1 in: " ++ p1 ++ " " ++ p2) res (dist p1 p2)

--distTest1 = distTest (1,1) (0,0) 2
--distTest2 = distTest (1,1) (0,1) 1
--distTest3 = distTest (1,1) (0,2) 2
--distTest4 = distTest (1,1) (1,2) 1
--distTest5 = distTest (1,1) (2,2) 2
--distTest6 = distTest (1,1) (2,1) 1
--distTest7 = distTest (1,1) (2,0) 2
--distTest8 = distTest (1,1) (1,0) 1
---- Test that horizontal and vertical returns distances > 1
--distTest9 = distTest (0,0) (2,0) 2
--distTest10 = distTest (2,0) (0,0) 2
--distTest11 = distTest (0,0) (0,2) 2
--distTest12 = distTest (0,2) (0,0) 2
--
--distTestList = TestList $ [ TestLabel "dist test 1" distTest1
--                          , TestLabel "dist test 2" distTest2
--                          , TestLabel "dist test 3" distTest2
--                          , TestLabel "dist test 4" distTest2
--                          , TestLabel "dist test 5" distTest2
--                          , TestLabel "dist test 6" distTest2
--                          , TestLabel "dist test 7" distTest2
--                          , TestLabel "dist test 8" distTest2
--                          , TestLabel "dist test 9" distTest2
--                          , TestLabel "dist test 10" distTest2
--                          , TestLabel "dist test 11" distTest2
--                          , TestLabel "dist test 12" distTest2
--                          ]

-- Test that fromList only returns a valid maze no matter the input
-- Give an empty list
positiveCells :: [((Int, Int), [Direction])]
positiveCells =[((0,0),[North,South,West]),((0,1),[North,South,West])
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

fromListTest1 :: Test
fromListTest1 = TestCase $ assertEqual "fromList test 1 " ((0,0),(4,4)) (bounds positiveMaze)

fromListTest2 :: Test
fromListTest2 = TestCase $ assertEqual "fromList test 2 " positiveValidCells (elems positiveMaze)

fromListTest3 :: Test
fromListTest3 = TestCase $ assertEqual "fromList test 3 " ((0,0),(0,0)) (bounds negativeMaze)

fromListTest4 :: Test
fromListTest4 = TestCase $ assertEqual "fromList test 4 " negativeValidCells (elems negativeMaze)

fromListTest5 :: Test
fromListTest5 = TestCase $ assertEqual "fromList test 5 " ((0,0),(4,4)) (bounds mixedMaze)

fromListTest6 :: Test
fromListTest6 = TestCase $ assertEqual "fromList test 6 " mixedValidCells (elems mixedMaze)

fromListTestList :: Test
fromListTestList = TestList [ TestLabel "fromList test 1" fromListTest1
                            , TestLabel "fromList test 2" fromListTest2
                            , TestLabel "fromList test 3" fromListTest3
                            , TestLabel "fromList test 4" fromListTest4
                            , TestLabel "fromList test 5" fromListTest5
                            , TestLabel "fromList test 6" fromListTest6
                            ]

-- Tests for runProg with different mazes

-- Test turns
runProgTest1 :: Test
runProgTest1 = TestCase $ assertEqual "TurnRight" (Stall ([(0,0)], East)) (runProg positiveMaze TurnRight)

runProgTest2 :: Test
runProgTest2 = TestCase $ assertEqual "TurnLeft" (Stall ([(0,0)], West)) (runProg positiveMaze TurnLeft)

-- Test if and wall
runProgTest3 :: Test
runProgTest3 = TestCase $ assertEqual "If test 1" (Stall ([(0,0)], East)) (runProg positiveMaze $ If (Wall Ahead) TurnRight TurnLeft)

runProgTest4 :: Test
runProgTest4 = TestCase $ assertEqual "If test 2" (Stall ([(0,0)], West)) (runProg positiveMaze $ If (Wall ToRight) TurnRight TurnLeft)

runProgTest5 :: Test
runProgTest5 = TestCase $ assertEqual "if test 3" (Stall ([(0,0)], East)) (runProg positiveMaze $ If (Wall ToLeft) TurnRight TurnLeft)

runProgTest6 :: Test
runProgTest6 = TestCase $ assertEqual "if test 4" (Stall ([(0,0)], East)) (runProg positiveMaze $ If (Wall Behind) TurnRight TurnLeft)

-- Test and
runProgTest7 :: Test
runProgTest7 = TestCase $ assertEqual "And test 1" (Stall ([(0,0)], East)) (runProg positiveMaze $ If (And (Wall Ahead) (Wall Behind)) TurnRight TurnLeft)

runProgTest8 :: Test
runProgTest8 = TestCase $ assertEqual "And test 2" (Stall ([(0,0)], West)) (runProg positiveMaze $ If (And (Wall Ahead) (Wall ToRight)) TurnRight TurnLeft)

runProgTest9 :: Test
runProgTest9 = TestCase $ assertEqual "And test 3" (Stall ([(0,0)], West)) (runProg positiveMaze $ If (And (Wall ToRight) (Wall ToRight)) TurnRight TurnLeft)

runProgTest10 :: Test
runProgTest10 = TestCase $ assertEqual "And test 4" (Stall ([(0,0)], West)) (runProg positiveMaze $ If (And (Wall ToRight) (Wall ToRight)) TurnRight TurnLeft)

-- Test not
runProgTest11 :: Test
runProgTest11 = TestCase $ assertEqual "Not test 1" (Stall ([(0,0)], West)) (runProg positiveMaze $ If (Not (Wall Ahead)) TurnRight TurnLeft)

runProgTest12 :: Test
runProgTest12 = TestCase $ assertEqual "Not Test 2" (Stall ([(0,0)], East)) (runProg positiveMaze $ If (Not (Wall ToRight)) TurnRight TurnLeft)

-- Test AtGoalPos
runProgTest13 :: Test
runProgTest13 = TestCase $ assertEqual "AtGoalPos test 1" (Stall ([(0,0)], West)) (runProg positiveMaze $ If AtGoalPos TurnRight TurnLeft)

runProgTest14 :: Test
runProgTest14 = TestCase $ assertEqual "AtGoalPos test 2" (Win ([(0,0)], East)) (runProg emptyMaze $ If AtGoalPos TurnRight TurnLeft)

-- Test Block
runProgTest15 :: Test
runProgTest15 = TestCase $ assertEqual "TurnRight and Forward" (Stall ([(0,0), (1,0)], East)) (runProg positiveMaze $ Block [TurnRight, Forward])

runProgTest16 :: Test
runProgTest16 = TestCase $ assertEqual "TurnRight, Forward and TurnLeft" (Stall ([(0,0), (1,0)], North)) (runProg positiveMaze $ Block [TurnRight, Forward, TurnLeft])

-- Test While
runProgTest17 :: Test
runProgTest17 = TestCase $ assertEqual "While test 1" (Stall ([(0,0)], East)) (runProg positiveMaze $ While (Wall Ahead) TurnLeft)

runProgTest18 :: Test
runProgTest18 = TestCase $ assertEqual "While test 2" (Stall ([(0,0)], North)) (runProg positiveMaze $ While (Wall ToRight) TurnRight)

-- Test Forward
runProgTest19 :: Test
runProgTest19 = TestCase $ assertEqual "Forward test 1" (Stall ([(0,0), (1,0), (2,0), (3,0)], East)) (runProg positiveMaze $ Block [TurnRight, Forward, Forward, Forward])

runProgTest20 :: Test
runProgTest20 = TestCase $ assertEqual "Forward test 2" (MoveError "Could not move North in position (0,0)" $ Robot (0,0) North []) (runProg positiveMaze Forward)

-- Test Backward
runProgTest21 :: Test
runProgTest21 = TestCase $ assertEqual "Backward test1" (Stall ([(0,0), (1,0), (2,0), (3,0)], West)) (runProg positiveMaze $ Block [TurnLeft, Backward, Backward, Backward])

runProgTest22 :: Test
runProgTest22 = TestCase $ assertEqual "Backward test 2" (MoveError "Could not move South in position (0,0)" $ Robot (0,0) North []) (runProg positiveMaze Backward)

-- Win the maze
runProgTest23 :: Test
runProgTest23 = TestCase $ assertEqual "Win the maze 1" (Win ([(0,0),(1,0),(2,0),(3,0),(4,0)
                                                            ,(3,0),(2,0),(1,0),(1,1),(2,1)
                                                            ,(2,2),(2,3),(3,3),(3,2),(3,1)
                                                            ,(4,1),(3,1),(3,2),(4,2),(3,2)
                                                            ,(3,3),(4,3),(4,4)],North)) 
                (runProg positiveMaze $ Block [While (Not AtGoalPos)
                                                 (If (Not $ Wall ToRight)
                                                   (Block [TurnRight, Forward])
                                                   (If (Not $ Wall Ahead) Forward TurnLeft))])

runProgTest24 :: Test
runProgTest24 = TestCase $ assertEqual "Win the maze 2" (Win ([(0,0)], North))
                (runProg emptyMaze $ Block [While (Not AtGoalPos)
                                                 (If (Not $ Wall ToRight)
                                                   (Block [TurnRight, Forward])
                                                   (If (Not $ Wall Ahead) Forward TurnLeft))])

runProgTest25 :: Test
runProgTest25 = TestCase $ assertEqual "Win the maze 3" (Win ([(0,0)], North))
                (runProg allWallsMaze $ Block [While (Not AtGoalPos)
                                                 (If (Not $ Wall ToRight)
                                                   (Block [TurnRight, Forward])
                                                   (If (Not $ Wall Ahead) Forward TurnLeft))])




runProgTestList :: Test
runProgTestList = TestList [ TestLabel "runProg test 1" runProgTest1
                           , TestLabel "runProg test 2" runProgTest2
                           , TestLabel "runProg test 3" runProgTest3
                           , TestLabel "runProg test 4" runProgTest4
                           , TestLabel "runProg test 5" runProgTest5
                           , TestLabel "runProg test 6" runProgTest6
                           , TestLabel "runProg test 7" runProgTest7
                           , TestLabel "runProg test 8" runProgTest8
                           , TestLabel "runProg test 9" runProgTest9
                           , TestLabel "runProg test 10" runProgTest10
                           , TestLabel "runProg test 11" runProgTest11
                           , TestLabel "runProg test 12" runProgTest12
                           , TestLabel "runProg test 13" runProgTest13
                           , TestLabel "runProg test 14" runProgTest14
                           , TestLabel "runProg test 15" runProgTest15
                           , TestLabel "runProg test 16" runProgTest16
                           , TestLabel "runProg test 17" runProgTest17
                           , TestLabel "runProg test 18" runProgTest18
                           , TestLabel "runProg test 19" runProgTest19
                           , TestLabel "runProg test 20" runProgTest20
                           , TestLabel "runProg test 21" runProgTest21
                           , TestLabel "runProg test 22" runProgTest22
                           , TestLabel "runProg test 23" runProgTest23
                           , TestLabel "runProg test 24" runProgTest24
                           , TestLabel "runProg test 25" runProgTest25
                           ]

runAllTests :: IO ()
runAllTests = do
  _ <- runTestTT oppositeDirTestList
  _ <- runTestTT rightTurnTestList
  _ <- runTestTT leftTurnTestList
  _ <- runTestTT rightleftTestList
  _ <- runTestTT moveTestList
  _ <- runTestTT fromListTestList
  _ <- runTestTT validMoveTestList
  _ <- runTestTT runProgTestList
  return ()
