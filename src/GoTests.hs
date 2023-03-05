module GoTests where
import GoTypesData
    ( Board,
      PlayerStats,
      PlayerID(..),
      stone,
      Stones(Blank) )
import GoConsts (pass, badInput)
import GoWork 
    ( boardState,
      currentPlayersStats,
      statsState,
      emptyBoard,
      getPos,
      updatePlayerPass,
      getPassCount,
      rowStates
    ) 

-- import HaskellGo

import Test.HUnit

-- Smaller boards are easier to test. All tests are using a 3x3 board
testBoardSize :: Int
testBoardSize = 3
testBoardSpaces :: Int
testBoardSpaces = testBoardSize*testBoardSize

testState :: ([PlayerStats], Board)
testState = (testStats1, testBoard1)

testStats1 :: [(PlayerID, (Int, Int))]
testStats1 = [(PB,(0,0)), (PW,(0,0))]

testStats2 :: [(PlayerID, (Int, Int))]
testStats2 = [(PB,(0,1)), (PW,(0,0))]

testStats3 :: [(PlayerID, (Int, Int))]
testStats3 = [(PB,(0,0)), (PW,(0,1))]

testBoard1 :: [(Char, (Int, Int))]
testBoard1 =
  [
    ('w',(0,0)),('w',(0,1)),('b',(0,2)),
    ('w',(0,3)),('b',(0,4)),('_',(0,5)),
    ('b',(0,6)),('_',(0,7)),('_',(0,8))
  ]
              
emptyBoardTest :: [(Char, (Int, Int))]
emptyBoardTest =
  [
    ('_',(0,0)),('_',(0,1)),('_',(0,2)),
    ('_',(0,3)),('_',(0,4)),('_',(0,5)),
    ('_',(0,6)),('_',(0,7)),('_',(0,8))
  ]

{-- 
//TODO Big test, the game "played" itself to ensure stones were captured and score
was updated and the newboard was produced.
--}



{--
All other tests
--}

testCurrentPlayerStats :: Test
testCurrentPlayerStats = "testCurrentPlayerStats" ~:
  TestList
      [
        currentPlayersStats PB testStats1 ~?= (PB,(0,0)),
        currentPlayersStats PW testStats1 ~?= (PW,(0,0))
      ]

testBoardState :: Test
testBoardState = "testBoardState" ~: boardState testState ~?= testBoard1

testStatsState :: Test
testStatsState = "testStatsState" ~: statsState testState ~?= testStats1

testEmptyBoard :: Test
testEmptyBoard = "testEmptyBoard" ~: emptyBoard 3 ~?= emptyBoardTest

{-- For reference for the following tests.
    If the position is 2, then the row should be 0.
    Position = 8 row = 2, p = 3 r = 1
    See tests below
                    0 1 2
                		1 2 3
                   _______  Positions avail:
              0 1 | _ _ _ |   0->2
              1 2 | _ _ _ |   3->5
              2 3 | _ _ _ |   6->8
  See the bottom of this file for what the test___' is and justification for
  using it here.
--}

testCurrentRow :: Test
testCurrentRow = "testCurrentRow" ~: 
  TestList
      [
        currentRow' 0 ~?= 0,
        currentRow' 2 ~?= 0,
        currentRow' 3 ~?= 1,
        currentRow' 5 ~?= 1,
        currentRow' 6 ~?= 2,
        currentRow' 8 ~?= 2
      ]

-- Using position on the board again, get the previous row by subtracting
-- what would be the current row by 1
-- p = 3 r = 1, r-1 = 0, previous row
-- p = 0 r = 0, r-1 = -1, doesn't exist but error checking is done elsewhere!
testPreviousRow :: Test
testPreviousRow = "testPreviousRow" ~: 
  TestList
      [
        previousRow' 0 ~?= -1,
        previousRow' 2 ~?= -1,
        previousRow' 3 ~?= 0,
        previousRow' 5 ~?= 0,
        previousRow' 6 ~?= 1,
        previousRow' 8 ~?= 1
      ]

-- Similar to above but adding 1 instead of subtracting.
-- In this case though, 3 is not a valid row but the game error checks for this
-- elsewhere!!
testNextRow :: Test
testNextRow = "testNextRow" ~: 
  TestList
      [
        nextRow' 0 ~?= 1,
        nextRow' 2 ~?= 1,
        nextRow' 3 ~?= 2,
        nextRow' 5 ~?= 2,
        nextRow' 6 ~?= 3, --DNE in test. Checked elsewhere!
        nextRow' 8 ~?= 3  --DNE in test. Checked elsewhere!
      ]

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
        rowLimit' 0 ~?= (0,2),
        rowLimit' 1 ~?= (3,5),
        rowLimit' 2 ~?= (6,8)
      ]

-- Check if the move the player made is legal or not using testBoard1
testLegalMove :: Test
testLegalMove = "testLegalMove" ~:
  TestList
      [
        legalMove' testBoard1 (-99, -99)  ~?= 1,
        legalMove' testBoard1 (3,3)       ~?= 1,
        legalMove' testBoard1 (-1,-1)     ~?= 2,
        legalMove' testBoard1 (1,1)       ~?= 3,
        legalMove' testBoard1 (9,9)       ~?= 4
      ]

testUpdatePlayerPass :: Test
testUpdatePlayerPass = "testUpdatePlayerPass" ~:
  TestList
          [
            updatePlayerPass PB (-99,-99) testStats1 ~?= testStats2,
            updatePlayerPass PW (-99,-99) testStats1 ~?= testStats3,
            updatePlayerPass PW (-99,-99) [] ~?= []
          ]

testGetPassCount :: Test
testGetPassCount = "testGetPassCount" ~:
  TestList 
      [
        getPassCount PB testState ~?= 0,
        getPassCount PW testState ~?= 0
      ]

testRowStates :: Test
testRowStates = "testRowStates" ~:
  TestList
      [
        rowStates (rowLimit' 0) testBoard1 ~?= " w w b",
        rowStates (rowLimit' 1) testBoard1 ~?= " w b _",
        rowStates (rowLimit' 2) testBoard1 ~?= " b _ _"
      ]




-- ***** NOTICE *****
-- The following functions are almost exactly the same as the the implemented
-- version but to properly test them, they must exist in this file utilizing
-- the test board size I've provided that's 3x3

rowLimit' :: Int -> (Int, Int)
rowLimit' row = (row*testBoardSize, row*testBoardSize+(testBoardSize-1))

-- This is the only one slightly changed due to a system function attempting
-- to be called
emptyBoard' :: Int -> Board
emptyBoard' n  =
  do
    [(stone Blank, (r,i)) | i <- [0..(n*n)-1], let r = currentRow' i]
    -- //TODO if i/boardsize /= whole number then r+1 else r


-- //TODO - Comment
currentRow' :: Int  -> Int
currentRow' i = i `div` testBoardSize

-- //TODO - Comment
previousRow' :: Int -> Int
previousRow' i = (i `div` testBoardSize)-1

-- //TODO - Comment
nextRow' :: Int -> Int
nextRow' i = (i `div` testBoardSize)+1


-- Checks to see if the user made a legal move on the current gameboard
legalMove' :: Board -> (Int, Int) -> Int
legalMove' board move | move == pass            = 1
                      | move == badInput        = 2
                      | isOccupied' board place = 3
                      | place > testBoardSpaces = 4
                      | otherwise               = 1
    where
      place = posCalc' move

-- Checks to see if the position passed in is occupied by another player
isOccupied' :: Board -> Int -> Bool
isOccupied' [] _                            = error "Empty game board detected"
isOccupied' (b:bs) pos
  | pos > testBoardSpaces                   = False
  | pos == getPos b && fst b /= stone Blank = True
  | pos /= getPos b                         = isOccupied' bs pos
  | not $ null b && null bs                 = False
  | otherwise                               = False

-- Quickly get the position in the Board[Position] list being changed
posCalc' :: (Int, Int) -> Int
posCalc' (x,y) = (x-1)+(y-1)*testBoardSize