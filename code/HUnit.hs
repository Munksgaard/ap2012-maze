import Test.HUnit
import World

testValidMove1 :: Test
testValidMove1 = TestCase (assertEqual "validMove (0,0) (0,0)" False (validMove testMaze1 (0,0) (0,0)))

testValidMove2 :: Test
testValidMove2 = TestCase (assertEqual "validMove (4,4) (4,4)" False (validMove testMaze1 (4,4) (4,4)))

testValidMove3 :: Test
testValidMove3 = TestCase (assertEqual "validMove (2,3) (2,3)" False (validMove testMaze1 (2,3) (2,3)))

testValidMove4 :: Test
testValidMove4 = TestCase (assertEqual "validMove (0,0) (1,0)" True (validMove testMaze1 (0,0) (1,0)))

testValidMove5 :: Test
testValidMove5 = TestCase (assertEqual "validMove (0,0) (0,1)" False (validMove testMaze1 (0,0) (0,1)))

testValidMove6 :: Test
testValidMove6 = TestCase (assertEqual "validMove (0,0) (0,-1)" False (validMove testMaze1 (0,0) (0,-1)))

testValidMove7 :: Test
testValidMove7 = TestCase (assertEqual "validMove (0,0) (-1, 0)" False (validMove testMaze1 (0,0) (-1, 0)))

testValidMove8 :: Test
testValidMove8 = TestCase (assertEqual "validMove (-1, 0) (0,0)" False (validMove testMaze1 (-1, 0) (0,0)))

testValidMove9 :: Test
testValidMove9 = TestCase (assertEqual "validMove (0, -1) (0,0)" False (validMove testMaze1 (0, -1) (0,0)))

testValidMove10 :: Test
testValidMove10 = TestCase (assertEqual "validMove (-2, -3) (-2, -4)" False (validMove testMaze1 (-2, -3) (-2, -4)))

testValidMove11 :: Test
testValidMove11 = TestCase (assertEqual "validMove (-3, -2) (-4, -2)" False (validMove testMaze1 (-3, -2) (-4, -2)))

testValidMove12 :: Test
testValidMove12 = TestCase (assertEqual "validMove (-5, -5) (-5, -4)" False (validMove testMaze1 (-5, -5) (-5, -4)))

testValidMove13 :: Test
testValidMove13 = TestCase (assertEqual "validMove (-5, -5) (-4, -5)" False (validMove testMaze1 (-5, -5) (-4, -5)))

testValidMove14 :: Test
testValidMove14 = TestCase (assertEqual "validMove (1,0) (2,1)" False (validMove testMaze1 (1,0) (2,1)))

testValidMove15 :: Test
testValidMove15 = TestCase (assertEqual "validMove (2,1) (1,0)" False (validMove testMaze1 (2,1) (1,0)))

testValidMove16 :: Test
testValidMove16 = TestCase (assertEqual "validMove (2,0) (1,1)" False (validMove testMaze1 (2,0) (1,1)))

testValidMove17 :: Test
testValidMove17 = TestCase (assertEqual "validMove (1,1) (2,0)" False (validMove testMaze1 (1,1) (2,0)))

testValidMove18 :: Test
testValidMove18 = TestCase (assertEqual "validMove (-2, 2) (-2, 3)" False (validMove testMaze1 (-2, 2) (-2, 3)))

testValidMove19 :: Test
testValidMove19 = TestCase (assertEqual "validMove (-2, 3) (-2, 2)" False (validMove testMaze1 (-2, 3) (-2, 2)))

testValidMove20 :: Test
testValidMove20 = TestCase (assertEqual "validMove (-2, 3) (-1, 3)" False (validMove testMaze1 (-2, 3) (-1, 3)))

testValidMove21 :: Test
testValidMove21 = TestCase (assertEqual "validMove (-1, 3) (-2, 3)" False (validMove testMaze1 (-1, 3) (-2, 3)))

testValidMove22 :: Test
testValidMove22 = TestCase (assertEqual "validMove (-2, 3) (-1, 2)" False (validMove testMaze1 (-2, 3) (-1, 2)))

testValidMove23 :: Test
testValidMove23 = TestCase (assertEqual "validMove (-2, 3) (-3, 4)" False (validMove testMaze1 (-2, 3) (-3, 4)))

testValidMove24 :: Test
testValidMove24 = TestCase (assertEqual "validMove (-2, 3) (-3, 4)" False (validMove testMaze1 (-2, 3) (-3, 4)))

testValidMove25 :: Test
testValidMove25 = TestCase (assertEqual "validMove (-3, 4) (-2, 3)" False (validMove testMaze1 (-3, 4) (-2, 3)))

testValidMove26 :: Test
testValidMove26 = TestCase (assertEqual "validMove (2, -2) (2, -3)" False (validMove testMaze1 (2, -2) (2, -3)))

testValidMove27 :: Test
testValidMove27 = TestCase (assertEqual "validMove (2, -3) (2, -2)" False (validMove testMaze1 (2, -3) (2, -2)))

testValidMove28 :: Test
testValidMove28 = TestCase (assertEqual "validMove (2, -2) (3, -2)" False (validMove testMaze1 (2, -2) (3, -2)))

testValidMove29 :: Test
testValidMove29 = TestCase (assertEqual "validMove (3, -2) (2, -2)" False (validMove testMaze1 (3, -2) (2, -2)))

testValidMove30 :: Test
testValidMove30 = TestCase (assertEqual "validMove (2, -2) (3, -1)" False (validMove testMaze1 (2, -2) (3, -1)))

testValidMove31 :: Test
testValidMove31 = TestCase (assertEqual "validMove (3, -1) (2, -2)" False (validMove testMaze1 (3, -1) (2, -2)))

testValidMove32 :: Test
testValidMove32 = TestCase (assertEqual "validMove (2, -2) (3, -3)" False (validMove testMaze1 (2, -2) (3, -3)))

testValidMove33 :: Test
testValidMove33 = TestCase (assertEqual "validMove (0, 0) (4, 0)" False (validMove testMaze1 (0, 0) (4, 0)))

testValidMove34 :: Test
testValidMove34 = TestCase (assertEqual "validMove (4, 0) (0, 0)" False (validMove testMaze1 (4, 0) (0, 0)))

testValidMove35 :: Test
testValidMove35 = TestCase (assertEqual "validMove (2, 1) (2, 4)" False (validMove testMaze1 (2, 1) (2, 4)))

testValidMove36 :: Test
testValidMove36 = TestCase (assertEqual "validMove (4, 2) (2, 1)" False (validMove testMaze1 (4,2) (2, 1)))

testValidMove37 :: Test
testValidMove37 = TestCase (assertEqual "validMove (6,6) (7,7)" False (validMove testMaze1 (6,6) (7,7)))

testValidMove38 :: Test
testValidMove38 = TestCase (assertEqual "validMove (7,7) (6,6)" False (validMove testMaze1 (7,7) (6,6)))

testValidMove39 :: Test
testValidMove39 = TestCase (assertEqual "validMove (8,8) (7,8)" False (validMove testMaze1 (8,8) (7,8)))

testValidMove40 :: Test
testValidMove40 = TestCase (assertEqual "validMove (7,8) (8,8)" False (validMove testMaze1 (7,8) (8,8)))

testValidMove41 :: Test
testValidMove41 = TestCase (assertEqual "validMove (8,8) (8,7)" False (validMove testMaze1 (8,8) (8,7)))

testValidMove42 :: Test
testValidMove42 = TestCase (assertEqual "validMove (0,4) (0,5)" False (validMove testMaze1 (0,4) (0,5)))

testValidMove43 :: Test
testValidMove43 = TestCase (assertEqual "validMove (0,4) (-1,4)" False (validMove testMaze1 (0,4) (-1,4)))

testValidMove44 :: Test
testValidMove44 = TestCase (assertEqual "validMove (4,0) (5,0)" False (validMove testMaze1 (4,0) (5,0)))

testValidMove45 :: Test
testValidMove45 = TestCase (assertEqual "validMove (4,0) (4,-1)" False (validMove testMaze1 (4,0) (4,-1)))

testValidMove46 :: Test
testValidMove46 = TestCase (assertEqual "validMove (4,4) (4,5)" False (validMove testMaze1 (4,4) (4,5)))

testValidMove47 :: Test
testValidMove47 = TestCase (assertEqual "validMove (4,4) (5,4)" False (validMove testMaze1 (4,4) (5,4)))

tests :: Test
tests = TestList[TestLabel "testValidMove1" testValidMove1
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
