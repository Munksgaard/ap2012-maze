module HUnitTest where
import World
import Test.HUnit
import Data.Array

-- Test that oppositeDir truly returns the opposite dir
oppositeDir_test1 = TestCase $ assertEqual "oppositeDir in: North" South (oppositeDir North)
oppositeDir_test2 = TestCase $ assertEqual "oppositeDir in: East" West (oppositeDir East)
oppositeDir_test3 = TestCase $ assertEqual "oppositeDir in: South" North (oppositeDir South)
oppositeDir_test4 = TestCase $ assertEqual "oppositeDir in: West" East (oppositeDir West)
-- No need for further testing here since this is all the possible arguments and
-- the function is deterministic
 
oppositeDirTestList = TestList [ TestLabel "oppositeDir_test1" oppositeDir_test1
                               , TestLabel "oppositeDir_test2" oppositeDir_test2
                               , TestLabel "oppositeDir_test3" oppositeDir_test3
                               , TestLabel "oppositeDir_test4" oppositeDir_test4
                               ]

-- Test that the function truly makes a right turn
rightTurn_test1 = TestCase $ assertEqual "rightTurn in: North" East (rightTurn North)
rightTurn_test2 = TestCase $ assertEqual "rightTurn in: East" South (rightTurn East)
rightTurn_test3 = TestCase $ assertEqual "rightTurn in: South" West (rightTurn South)
rightTurn_test4 = TestCase $ assertEqual "rightTurn in: West" North (rightTurn West)

-- Test that the function wraps after 4 rightturns
rightTurn_test5 = TestCase $ assertEqual "rightTurn wrap" North 
  (rightTurn $ rightTurn $ rightTurn $ rightTurn North)

rightTurnTestList = TestList [ TestLabel "rightTurn_test1" rightTurn_test1
                             , TestLabel "rightTurn_test2" rightTurn_test2
                             , TestLabel "rightTurn_test3" rightTurn_test3
                             , TestLabel "rightTurn_test4" rightTurn_test4
                             , TestLabel "rightTurn_test5" rightTurn_test5
                             ]

 -- Test that the function truly makes a left turn
leftTurn_test1 = TestCase $ assertEqual "leftTurn in: North" West (leftTurn North)
leftTurn_test2 = TestCase $ assertEqual "leftTurn in: East" North (leftTurn East)
leftTurn_test3 = TestCase $ assertEqual "leftTurn in: South" East (leftTurn South)
leftTurn_test4 = TestCase $ assertEqual "leftTurn in: West" South (leftTurn West)
-- All possible values

-- Test that the function wraps after 4 leftturns
leftTurn_test5 = TestCase $ assertEqual "leftTurn wrap" North 
  (leftTurn $ leftTurn $ leftTurn $ leftTurn North)

leftTurnTestList = TestList [ TestLabel "leftTurn_test1" leftTurn_test1
                             , TestLabel "leftTurn_test2" leftTurn_test2
                             , TestLabel "leftTurn_test3" leftTurn_test3
                             , TestLabel "leftTurn_test4" leftTurn_test4
                             , TestLabel "leftTurn_test5" leftTurn_test5
                             ]

-- Test that right- and leftTurns are truly opposite
rightleft_test1 = TestCase $ assertEqual "right/left Test1 in: North" 
  (rightTurn North) (oppositeDir (leftTurn North))
rightleft_test2 = TestCase $ assertEqual "right/left Test2 in: East" 
  (rightTurn East) (oppositeDir (leftTurn East))
rightleft_test3 = TestCase $ assertEqual "right/left Test3 in: South" 
  (rightTurn South) (oppositeDir (leftTurn South))
rightleft_test4 = TestCase $ assertEqual "right/left Test4 in: West" 
  (rightTurn West) (oppositeDir (leftTurn West))

rightleftTestList = TestList [ TestLabel "right/left test1" rightleft_test1
                    , TestLabel "right/left test2" rightleft_test2
                    , TestLabel "right/left test3" rightleft_test3
                    , TestLabel "right/left test4" rightleft_test4
                    ]

-- The move function asserts that the position and direction is a validmove.
-- Test move in all directions
move_test1 = TestCase $ assertEqual "move test1 in: West (1,0)" (0,0) (move West (1,0))
move_test2 = TestCase $ assertEqual "move test1 in: North (1,0)" (1,1) (move North (1,0))
move_test3 = TestCase $ assertEqual "move test1 in: East (1,0)" (2,0) (move East (1,0))

move_test4 = TestCase $ assertEqual "move test1 in: North (0,1)" (0,2) (move North (0,1))
move_test5 = TestCase $ assertEqual "move test1 in: East (0,1)" (1,1) (move East (0,1))
move_test6 = TestCase $ assertEqual "move test1 in: South (0,1)" (0,0) (move South (0,1))

move_test7 = TestCase $ assertEqual "move test1 in: North (1,1)" (1,2) (move North (1,1))
move_test8 = TestCase $ assertEqual "move test1 in: East (1,1)" (2,1) (move East (1,1))
move_test9 = TestCase $ assertEqual "move test1 in: South (1,1)" (1,0) (move South (1,1))
move_test10 = TestCase $ assertEqual "move test1 in: West (1,1)" (0,1) (move West (1,1))

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
positiveValidIndices = [ (0,0),(0,1),(0,2),(0,3),(0,4),(1,0),(1,1),(1,2),(1,3),(1,4)
                       , (2,0),(2,1),(2,2),(2,3),(2,4),(3,0),(3,1),(3,2),(3,3),(3,4)
                       , (4,0),(4,1),(4,2),(4,3),(4,4)]
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

positiveMaze = fromList positiveCells

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
negativeValidIndices = [(0,0)]
negativeValidCells = [[North,South,West]]
negativeMaze = fromList negativeCells

mixedCells = positiveCells ++ negativeCells
mixedValidCells = positiveValidCells
mixedMaze = fromList mixedCells

fromList_test1 = TestCase $ assertEqual "fromList test 1 " ((0,0),(4,4)) (bounds positiveMaze)
fromList_test2 = TestCase $ assertEqual "fromList test 2 " positiveValidCells (elems positiveMaze)
fromList_test3 = TestCase $ assertEqual "fromList test 3 " ((0,0),(0,0)) (bounds negativeMaze)
fromList_test4 = TestCase $ assertEqual "fromList test 4 " negativeValidCells (elems negativeMaze)
fromList_test5 = TestCase $ assertEqual "fromList test 5 " ((0,0),(4,4)) (bounds mixedMaze)
fromList_test6 = TestCase $ assertEqual "fromList test 6 " mixedValidCells (elems mixedMaze)

fromListTestList = TestList [ TestLabel "fromList test 1" fromList_test1
                            , TestLabel "fromList test 2" fromList_test2
                            , TestLabel "fromList test 3" fromList_test3
                            , TestLabel "fromList test 4" fromList_test4
                            , TestLabel "fromList test 5" fromList_test5
                            , TestLabel "fromList test 6" fromList_test6
                            ]

runAllTests = do
  _ <- runTestTT oppositeDirTestList
  _ <- runTestTT rightTurnTestList
  _ <- runTestTT leftTurnTestList
  _ <- runTestTT rightleftTestList
  _ <- runTestTT moveTestList
  _ <- runTestTT fromListTestList
  return ()
