module GoTests where

import GoTypesData
import GoWork
import GoCapture
import Test.HUnit ( (~?=), (~:), Test(TestList) )
import GoConsts


-- Smaller boards are easier to test. All tests are using a 3x3 board
tBSize :: Int
tBSize = 3

-- Empty stats used in the tests
testStats1 :: [(PlayerID, (Int, Int))]
testStats1 = [(PB,(0,0)), (PW,(0,0))]

-- Modified PB pass value used to check in updating players pass value
testStats2 :: [(PlayerID, (Int, Int))]
testStats2 = [(PB,(0,1)), (PW,(0,0))]

-- Modified PW pass value used to check in updating players pass value
testStats3 :: [(PlayerID, (Int, Int))]
testStats3 = [(PB,(0,0)), (PW,(0,1))]

-- The state of the board when testing the larger functions.
testState :: ([PlayerStats], Board)
testState = (testStats1, testBoard1)

{-- 
//TODO Big test, the game "played" itself to ensure stones were captured and score
was updated and the newBoard was produced.
 This cannot be completed yet due to the program still being a WIP.
--}



{--
All other tests, the first few of which are fairly simple.
--}

-- Test that ensures that currentPlayerStats properly returns expected values
testCurrentPlayerStats :: Test
testCurrentPlayerStats = "testCurrentPlayerStats" ~:
  TestList
      [
        currentPlayersStats PB testStats1 ~?= (PB,(0,0)),
        currentPlayersStats PW testStats1 ~?= (PW,(0,0))
      ]

-- Test to verify the getter `boardState` returns the proper information
testBoardState :: Test
testBoardState = "testBoardState" ~: boardState testState ~?= testBoard1

-- Test to verify the getter for `statsState` returns the proper information
testStatsState :: Test
testStatsState = "testStatsState" ~: statsState testState ~?= testStats1

-- Test to verify that emptyBoard (with our test size) returns and matches the
-- prepared version above
testEmptyBoard :: Test
testEmptyBoard = "testEmptyBoard" ~: emptyBoard tBSize ~?= emptyBoardTest

{-- To reference for the following tests.
    If the position is 2, then the row should be 0.
    Position = 8 row = 2, p = 3 r = 1
    See tests below

             index  0 1 2
      players view  1 2 3
                   _______  Positions avail from 0:
              0 1 | _ _ _ |   0->2 
              1 2 | _ _ _ |   3->5
              2 3 | _ _ _ |   6->8
  See the bottom of this file for what the test___' is and justification for
  using it here.
--}

-- Verify currentRow performs the correct arithmetic for the position passed in 
testCurrentRow :: Test
testCurrentRow = "testCurrentRow" ~:
  TestList
      [
        currentRow tBSize 0 ~?= 0,
        currentRow tBSize 2 ~?= 0,
        currentRow tBSize 3 ~?= 1,
        currentRow tBSize 5 ~?= 1,
        currentRow tBSize 6 ~?= 2,
        currentRow tBSize 8 ~?= 2
      ]

-- Using position on the board again, get the previous row by subtracting
-- what would be the current row by 1
-- p = 3 r = 1, r-1 = 0, previous row
-- p = 0 r = 0, r-1 = -1, doesn't exist but error checking is done elsewhere!
testPreviousRow :: Test
testPreviousRow = "testPreviousRow" ~:
  TestList
      [
        previousRow tBSize 0 ~?= -1,
        previousRow tBSize 2 ~?= -1,
        previousRow tBSize 3 ~?= 0,
        previousRow tBSize 5 ~?= 0,
        previousRow tBSize 6 ~?= 1,
        previousRow tBSize 8 ~?= 1
      ]

-- Similar to above but adding 1 instead of subtracting.
-- In this case though, 3 is not a valid row but the game error checks for this
-- elsewhere!!
testNextRow :: Test
testNextRow = "testNextRow" ~:
  TestList
      [
        nextRow tBSize 0 ~?= 1,
        nextRow tBSize 2 ~?= 1,
        nextRow tBSize 3 ~?= 2,
        nextRow tBSize 5 ~?= 2,
        nextRow tBSize 6 ~?= 3, --DNE in test. Checked elsewhere!
        nextRow tBSize 8 ~?= 3  --DNE in test. Checked elsewhere!
      ]

-- Test the getNext function to see if it returns actual positions correctly
-- or no position if it wasn't found.
testGetNext :: Test
testGetNext = "testGetNext" ~:
  TestList
      [
        getNext 0 testBoard1 ~?= ('w',(0,1)),
        getNext 3 testBoard1 ~?= ('b',(1,4)),
        getNext 7 testBoard1 ~?= ('_',(2,8)),
        getNext 8 testBoard1 ~?= endOfBoard
      ]
-- testBoard1 :: [(Char, (Int, Int))]
-- testBoard1 =
--   [
--     ('w',(0,0)),('w',(0,1)),('b',(0,2)),
--     ('w',(1,3)),('b',(1,4)),('_',(1,5)),
--     ('b',(2,6)),('_',(2,7)),('_',(2,8))
--   ]

-- Get the position in the list based on the position set being analyzed
testGetPosition :: Test
testGetPosition = "testGetPosition" ~:
  TestList
      [
        getPos ('_', (0, 0)) ~?= 0,
        getPos ('_', (0, 1)) ~?= 1,
        getPos ('_', (0, 2)) ~?= 2,
        getPos ('_', (1, 3)) ~?= 3,
        getPos ('_', (1, 4)) ~?= 4,
        getPos ('_', (1, 5)) ~?= 5,
        getPos ('_', (2, 6)) ~?= 6,
        getPos ('_', (2, 7)) ~?= 7,
        getPos ('_', (2, 8)) ~?= 8
      ]

-- Get the bounds of the current row in our 3x3 grid
testRowLimit :: Test
testRowLimit = "testRowLimit" ~:
  TestList
      [
        rowLimit tBSize 0 ~?= (0,2),
        rowLimit tBSize 1 ~?= (3,5),
        rowLimit tBSize 2 ~?= (6,8)
      ]

-- Check to ensure the formula for posCalc is operating correctly for various
-- board sizes and positions. User is required to enter in x,y values > 0.
testPosCalc :: Test
testPosCalc = "testPosCalc" ~:
  TestList
    [
      posCalc (9,9) 9 ~?= 80,
      posCalc (2,3) 3 ~?= 7,
      posCalc (3,3) 3 ~?= 8,
      posCalc (1,1) 3 ~?= 0,
      posCalc (4,8) 19 ~?= 136,
      posCalc (1,1) 2 ~?= 0,
      posCalc (2,1) 2 ~?= 1
    ]

-- Check if the move the player made is legal or not using testBoard1
testLegalMove :: Test
testLegalMove = "testLegalMove" ~:
  TestList
      [
        legalMove tBSize testBoard1 (-99, -99)  ~?= 1,
        legalMove tBSize testBoard1 (3,3)       ~?= 1,
        legalMove tBSize testBoard1 (-1,-1)     ~?= 2,
        legalMove tBSize testBoard1 (1,1)       ~?= 3,
        legalMove tBSize testBoard1 (9,9)       ~?= 4
      ]

-- Verify the program has updated the players pass with respect to playerID
testUpdatePlayerPass :: Test
testUpdatePlayerPass = "testUpdatePlayerPass" ~:
  TestList
          [
            updatePlayerPass PB (-99,-99) testStats1 ~?= testStats2,
            updatePlayerPass PW (-99,-99) testStats1 ~?= testStats3,
            updatePlayerPass PW (-99,-99) [] ~?= []
          ]

-- Ensure the getPassCount returns the correct value with respect to playerID
testGetPassCount :: Test
testGetPassCount = "testGetPassCount" ~:
  TestList
      [
        getPassCount PB testState ~?= 0,
        getPassCount PW testState ~?= 0
      ]

-- Ensures rowStates returns the correct string from our testBoard1
testRowStates :: Test
testRowStates = "testRowStates" ~:
  TestList
      [
        rowStates (rowLimit tBSize 0) testBoard1 ~?= " w w b",
        rowStates (rowLimit tBSize 1) testBoard1 ~?= " w b _",
        rowStates (rowLimit tBSize 2) testBoard1 ~?= " b _ _"
      ]

      --        index  0 1 2
      -- players view  1 2 3
      --              _______  Positions avail from 0:
      --         0 1 | w w b |   0->2
      --         1 2 | w b _ |   3->5
      --         2 3 | b _ _ |   6->8

-- Test to see if the identify units works properly. This is only for units,
-- and does not work for single stones. Examining single stones is easily done
-- with functions already written.
testIdentifyUnits :: Test
testIdentifyUnits = "testIdentifyUnits" ~:
  TestList
      [
        identifyUnits tBSize testBoard1 ~?= [[0,1,3]],
        identifyUnits tBSize testBoard2 ~?= [[0,1,2,3,5,6,8],[4,7]],
        identifyUnits tBSize testBoard3 ~?= [[0,1,2,3,6]],
        identifyUnits tBSize testBoard4 ~?= [[0,1,2,3],[7,8]],
        identifyUnits tBSize testBoard5 ~?= [[1,3,4]],
        identifyUnits tBSize testBoard6 ~?= [[]]
      ]

-- Test to check the board for any stones that are solo and have been captured
testCappedSingles :: Test
testCappedSingles = "testCappedSingles" ~:
  TestList
      [
        cappedSingles tBSize units5 PW testBoard5 testBoard5 ~?= [0],
        cappedSingles tBSize units6 PW testBoard6 testBoard6 ~?= [],
        cappedSingles tBSize units6 PB testBoard6 testBoard6 ~?= [],
        cappedSingles tBSize units7 PW testBoard7 testBoard7 ~?= [0,8],
        cappedSingles tBSize units8 PB testBoard8 testBoard8 ~?= [4]
      ]
  where
    units5 = identifyUnits tBSize testBoard5
    units6 = identifyUnits tBSize testBoard6
    units7 = identifyUnits tBSize testBoard7
    units8 = identifyUnits tBSize testBoard8

-- Test to check if the units can properly identified
-- as captured or not.
testCappedUnits :: Test
testCappedUnits = "testCappedUnits" ~:
  TestList
      [
        cappedUnits tBSize units1 PW testBoard1 testBoard1    ~?= [0,1,3],
        cappedUnits tBSize units2 PW testBoard2 testBoard2    ~?= [4,7],
        cappedUnits tBSize units3 PB testBoard3 testBoard3    ~?= [],
        cappedUnits tBSize units3_1 PB testBoard3_1 testBoard3_1    ~?= [],
        cappedUnits tBSize units4 PB testBoard4 testBoard4    ~?= [],
        cappedUnits tBSize units4 PW testBoard4 testBoard4    ~?= [],
        cappedUnits tBSize units7 PW testBoard7 testBoard7    ~?= [],
        cappedUnits tBSize units8 PB testBoard8 testBoard8    ~?= [],
        cappedUnits tBSize units9 PW testBoard9 testBoard9    ~?= [0,3],
        cappedUnits tBSize units10 PW testBoard10 testBoard10 ~?= [5,8]
      ]
      where
        units1 = identifyUnits tBSize testBoard1
        units2 = identifyUnits tBSize testBoard2
        units3 = identifyUnits tBSize testBoard3
        units3_1 = identifyUnits tBSize testBoard3_1
        units4 = identifyUnits tBSize testBoard4
        units7 = identifyUnits tBSize testBoard7
        units8 = identifyUnits tBSize testBoard8
        units9 = identifyUnits tBSize testBoard9
        units10 = identifyUnits tBSize testBoard10


--Test to verify that all stones that should be captured have been identified
-- and returned
testCapturedStones :: Test
testCapturedStones = "testCapturedStones" ~:
  TestList
    [
      capturedStones tBSize PW emptyBoardTest ~?= [],
      capturedStones tBSize PB emptyBoardTest ~?= [],
      capturedStones tBSize PW testBoard1     ~?= [0,1,3],
      capturedStones tBSize PW testBoard2     ~?= [4,7],
      capturedStones tBSize PB testBoard2     ~?= [0,1,2,3,5,6,8],
      capturedStones tBSize PB testBoard3     ~?= [],
      capturedStones tBSize PB testBoard3_1   ~?= [],
      capturedStones tBSize PB testBoard4     ~?= [],
      capturedStones tBSize PW testBoard4     ~?= [],
      capturedStones tBSize PB testBoard7     ~?= [],
      capturedStones tBSize PW testBoard7     ~?= [0,8],
      capturedStones tBSize PB testBoard8     ~?= [4],
      capturedStones tBSize PB testBoard9     ~?= [],
      capturedStones tBSize PW testBoard9     ~?= [0,3],
      capturedStones tBSize PB testBoard10    ~?= [],
      capturedStones tBSize PW testBoard10    ~?= [5,8],
      capturedStones tBSize PB testBoard11    ~?= [],
      capturedStones tBSize PW testBoard11    ~?= [0,5,8]
    ]


-- All the test boards used in tests above
testBoard1 :: [(Char, (Int, Int))]
testBoard1 =
  [
    ('w',(0,0)),('w',(0,1)),('b',(0,2)),
    ('w',(1,3)),('b',(1,4)),('_',(1,5)),
    ('b',(2,6)),('_',(2,7)),('_',(2,8))
  ]

testBoard2 :: [(Char, (Int, Int))]
testBoard2 =
  [
    ('b',(0,0)),('b',(0,1)),('b',(0,2)),
    ('b',(1,3)),('w',(1,4)),('b',(1,5)),
    ('b',(2,6)),('w',(2,7)),('b',(2,8))
  ]

testBoard3 :: [(Char, (Int, Int))]
testBoard3 =
  [
    ('b',(0,0)),('b',(0,1)),('b',(0,2)),
    ('b',(1,3)),('_',(1,4)),('_',(1,5)),
    ('b',(2,6)),('_',(2,7)),('_',(2,8))
  ]

testBoard3_1 :: [(Char, (Int, Int))]
testBoard3_1 =
  [
    ('b',(0,0)),('b',(0,1)),('b',(0,2)),
    ('b',(1,3)),('b',(1,4)),('_',(1,5)),
    ('b',(2,6)),('b',(2,7)),('_',(2,8))
  ]

testBoard4 :: [(Char, (Int, Int))]
testBoard4 =
  [
    ('b',(0,0)),('b',(0,1)),('b',(0,2)),
    ('b',(1,3)),('_',(1,4)),('_',(1,5)),
    ('_',(2,6)),('w',(2,7)),('w',(2,8))
  ]

testBoard5 :: [(Char, (Int, Int))]
testBoard5 =
  [
    ('w',(0,0)),('b',(0,1)),('_',(0,2)),
    ('b',(1,3)),('b',(1,4)),('_',(1,5)),
    ('_',(2,6)),('_',(2,7)),('w',(2,8))
  ]

testBoard6 :: [(Char, (Int, Int))]
testBoard6 =
  [
    ('_',(0,0)),('b',(0,1)),('_',(0,2)),
    ('w',(1,3)),('_',(1,4)),('b',(1,5)),
    ('_',(2,6)),('w',(2,7)),('_',(2,8))
  ]

testBoard7 :: [(Char, (Int, Int))]
testBoard7 =
  [
    ('w',(0,0)),('b',(0,1)),('_',(0,2)),
    ('b',(1,3)),('_',(1,4)),('b',(1,5)),
    ('_',(2,6)),('b',(2,7)),('w',(2,8))
  ]

testBoard8 :: [(Char, (Int, Int))]
testBoard8 =
  [
    ('_',(0,0)),('w',(0,1)),('_',(0,2)),
    ('w',(1,3)),('b',(1,4)),('w',(1,5)),
    ('_',(2,6)),('w',(2,7)),('_',(2,8))
  ]

testBoard9 :: [(Char, (Int, Int))]
testBoard9 =
  [
    ('w',(0,0)),('b',(0,1)),('_',(0,2)),
    ('w',(1,3)),('b',(1,4)),('_',(1,5)),
    ('b',(2,6)),('_',(2,7)),('_',(2,8))
  ]

testBoard10 :: [(Char, (Int, Int))]
testBoard10 =
  [
    ('_',(0,0)),('_',(0,1)),('b',(0,2)),
    ('_',(1,3)),('b',(1,4)),('w',(1,5)),
    ('_',(2,6)),('b',(2,7)),('w',(2,8))
  ]

testBoard11 :: [(Char, (Int, Int))]
testBoard11 =
  [
    ('w',(0,0)),('b',(0,1)),('b',(0,2)),
    ('b',(1,3)),('b',(1,4)),('w',(1,5)),
    ('_',(2,6)),('b',(2,7)),('w',(2,8))
  ]

-- An empty board used to verify certain situations in tests
emptyBoardTest :: [(Char, (Int, Int))]
emptyBoardTest =
  [
    ('_',(0,0)),('_',(0,1)),('_',(0,2)),
    ('_',(1,3)),('_',(1,4)),('_',(1,5)),
    ('_',(2,6)),('_',(2,7)),('_',(2,8))
  ]