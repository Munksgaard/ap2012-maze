module HUnitTest where
import World
import Test.HUnit
import Data.Array

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
oppositeDir_test1 :: Test
oppositeDir_test1 = TestCase $ assertEqual "oppositeDir in: North" South (oppositeDir North)

oppositeDir_test2 :: Test
oppositeDir_test2 = TestCase $ assertEqual "oppositeDir in: East" West (oppositeDir East)

oppositeDir_test3 :: Test
oppositeDir_test3 = TestCase $ assertEqual "oppositeDir in: South" North (oppositeDir South)

oppositeDir_test4 :: Test
oppositeDir_test4 = TestCase $ assertEqual "oppositeDir in: West" East (oppositeDir West)
-- No need for further testing here since this is all the possible arguments and
-- the function is deterministic
 
oppositeDirTestList :: Test
oppositeDirTestList = TestList [ TestLabel "oppositeDir_test1" oppositeDir_test1
                               , TestLabel "oppositeDir_test2" oppositeDir_test2
                               , TestLabel "oppositeDir_test3" oppositeDir_test3
                               , TestLabel "oppositeDir_test4" oppositeDir_test4
                               ]

-- Test that the function truly makes a right turn
rightTurn_test1 :: Test
rightTurn_test1 = TestCase $ assertEqual "rightTurn in: North" East (rightTurn North)

rightTurn_test2 :: Test
rightTurn_test2 = TestCase $ assertEqual "rightTurn in: East" South (rightTurn East)

rightTurn_test3 :: Test
rightTurn_test3 = TestCase $ assertEqual "rightTurn in: South" West (rightTurn South)

rightTurn_test4 :: Test
rightTurn_test4 = TestCase $ assertEqual "rightTurn in: West" North (rightTurn West)

-- Test that the function wraps after 4 rightturns
rightTurn_test5 :: Test
rightTurn_test5 = TestCase $ assertEqual "rightTurn wrap" North 
  (rightTurn $ rightTurn $ rightTurn $ rightTurn North)

rightTurnTestList :: Test
rightTurnTestList = TestList [ TestLabel "rightTurn_test1" rightTurn_test1
                             , TestLabel "rightTurn_test2" rightTurn_test2
                             , TestLabel "rightTurn_test3" rightTurn_test3
                             , TestLabel "rightTurn_test4" rightTurn_test4
                             , TestLabel "rightTurn_test5" rightTurn_test5
                             ]

 -- Test that the function truly makes a left turn
leftTurn_test1 :: Test
leftTurn_test1 = TestCase $ assertEqual "leftTurn in: North" West (leftTurn North)

leftTurn_test2 :: Test
leftTurn_test2 = TestCase $ assertEqual "leftTurn in: East" North (leftTurn East)

leftTurn_test3 :: Test
leftTurn_test3 = TestCase $ assertEqual "leftTurn in: South" East (leftTurn South)

leftTurn_test4 :: Test
leftTurn_test4 = TestCase $ assertEqual "leftTurn in: West" South (leftTurn West)
-- All possible values

-- Test that the function wraps after 4 leftturns
leftTurn_test5 :: Test
leftTurn_test5 = TestCase $ assertEqual "leftTurn wrap" North 
  (leftTurn $ leftTurn $ leftTurn $ leftTurn North)

leftTurnTestList :: Test
leftTurnTestList = TestList [ TestLabel "leftTurn_test1" leftTurn_test1
                             , TestLabel "leftTurn_test2" leftTurn_test2
                             , TestLabel "leftTurn_test3" leftTurn_test3
                             , TestLabel "leftTurn_test4" leftTurn_test4
                             , TestLabel "leftTurn_test5" leftTurn_test5
                             ]

-- Test that right- and leftTurns are truly opposite
rightleft_test1 :: Test
rightleft_test1 = TestCase $ assertEqual "right/left Test1 in: North" 
  (rightTurn North) (oppositeDir (leftTurn North))

rightleft_test2 :: Test
rightleft_test2 = TestCase $ assertEqual "right/left Test2 in: East" 
  (rightTurn East) (oppositeDir (leftTurn East))

rightleft_test3 :: Test
rightleft_test3 = TestCase $ assertEqual "right/left Test3 in: South" 
  (rightTurn South) (oppositeDir (leftTurn South))

rightleft_test4 :: Test
rightleft_test4 = TestCase $ assertEqual "right/left Test4 in: West" 
  (rightTurn West) (oppositeDir (leftTurn West))

rightleftTestList :: Test
rightleftTestList = TestList [ TestLabel "right/left test1" rightleft_test1
                    , TestLabel "right/left test2" rightleft_test2
                    , TestLabel "right/left test3" rightleft_test3
                    , TestLabel "right/left test4" rightleft_test4
                    ]

-- The move function asserts that the position and direction is a validmove.
-- Test move in all directions
move_test1 :: Test
move_test1 = TestCase $ assertEqual "move test1 in: West (1,0)" (0,0) (move West (1,0))

move_test2 :: Test
move_test2 = TestCase $ assertEqual "move test1 in: North (1,0)" (1,1) (move North (1,0))

move_test3 :: Test
move_test3 = TestCase $ assertEqual "move test1 in: East (1,0)" (2,0) (move East (1,0))

move_test4 :: Test
move_test4 = TestCase $ assertEqual "move test1 in: North (0,1)" (0,2) (move North (0,1))

move_test5 :: Test
move_test5 = TestCase $ assertEqual "move test1 in: East (0,1)" (1,1) (move East (0,1))

move_test6 :: Test
move_test6 = TestCase $ assertEqual "move test1 in: South (0,1)" (0,0) (move South (0,1))

move_test7 :: Test
move_test7 = TestCase $ assertEqual "move test1 in: North (1,1)" (1,2) (move North (1,1))

move_test8 :: Test
move_test8 = TestCase $ assertEqual "move test1 in: East (1,1)" (2,1) (move East (1,1))

move_test9 :: Test
move_test9 = TestCase $ assertEqual "move test1 in: South (1,1)" (1,0) (move South (1,1))

move_test10 :: Test
move_test10 = TestCase $ assertEqual "move test1 in: West (1,1)" (0,1) (move West (1,1))

moveTestList :: Test
moveTestList = TestList $ [ TestLabel "move test1" move_test1
                          , TestLabel "move test2" move_test2
                          , TestLabel "move test3" move_test3
                          , TestLabel "move test4" move_test4
                          , TestLabel "move test5" move_test5
                          , TestLabel "move test6" move_test6
                          , TestLabel "move test7" move_test7
                          , TestLabel "move test8" move_test8
                          , TestLabel "move test9" move_test9
                          , TestLabel "move test10" move_test10
                          ]

-- Test each direction horizontal, vertical and diagonal (while we're at it)
--distTest :: Position -> Position -> Test
--distTest p1 p2 res = TestCase $ assertEqual ("dist test1 in: " ++ p1 ++ " " ++ p2) res (dist p1 p2)

--dist_test1 = distTest (1,1) (0,0) 2
--dist_test2 = distTest (1,1) (0,1) 1
--dist_test3 = distTest (1,1) (0,2) 2
--dist_test4 = distTest (1,1) (1,2) 1
--dist_test5 = distTest (1,1) (2,2) 2
--dist_test6 = distTest (1,1) (2,1) 1
--dist_test7 = distTest (1,1) (2,0) 2
--dist_test8 = distTest (1,1) (1,0) 1
---- Test that horizontal and vertical returns distances > 1
--dist_test9 = distTest (0,0) (2,0) 2
--dist_test10 = distTest (2,0) (0,0) 2
--dist_test11 = distTest (0,0) (0,2) 2
--dist_test12 = distTest (0,2) (0,0) 2
--
--distTestList = TestList $ [ TestLabel "dist test 1" dist_test1
--                          , TestLabel "dist test 2" dist_test2
--                          , TestLabel "dist test 3" dist_test2
--                          , TestLabel "dist test 4" dist_test2
--                          , TestLabel "dist test 5" dist_test2
--                          , TestLabel "dist test 6" dist_test2
--                          , TestLabel "dist test 7" dist_test2
--                          , TestLabel "dist test 8" dist_test2
--                          , TestLabel "dist test 9" dist_test2
--                          , TestLabel "dist test 10" dist_test2
--                          , TestLabel "dist test 11" dist_test2
--                          , TestLabel "dist test 12" dist_test2
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

mixedCells :: [((Int, Int), [Direction])]
mixedCells = positiveCells ++ negativeCells

mixedValidCells :: [[Direction]]
mixedValidCells = positiveValidCells

mixedMaze :: Maze
mixedMaze = fromList mixedCells

fromList_test1 :: Test
fromList_test1 = TestCase $ assertEqual "fromList test 1 " ((0,0),(4,4)) (bounds positiveMaze)

fromList_test2 :: Test
fromList_test2 = TestCase $ assertEqual "fromList test 2 " positiveValidCells (elems positiveMaze)

fromList_test3 :: Test
fromList_test3 = TestCase $ assertEqual "fromList test 3 " ((0,0),(0,0)) (bounds negativeMaze)

fromList_test4 :: Test
fromList_test4 = TestCase $ assertEqual "fromList test 4 " negativeValidCells (elems negativeMaze)

fromList_test5 :: Test
fromList_test5 = TestCase $ assertEqual "fromList test 5 " ((0,0),(4,4)) (bounds mixedMaze)

fromList_test6 :: Test
fromList_test6 = TestCase $ assertEqual "fromList test 6 " mixedValidCells (elems mixedMaze)

fromListTestList :: Test
fromListTestList = TestList [ TestLabel "fromList test 1" fromList_test1
                            , TestLabel "fromList test 2" fromList_test2
                            , TestLabel "fromList test 3" fromList_test3
                            , TestLabel "fromList test 4" fromList_test4
                            , TestLabel "fromList test 5" fromList_test5
                            , TestLabel "fromList test 6" fromList_test6
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
  return ()
