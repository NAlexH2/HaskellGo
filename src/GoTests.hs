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
      rowStates,
      identifyUnits,
      getPID
    )

-- import HaskellGo

import Test.HUnit
import Data.List ( union, nub, sort )

-- Smaller boards are easier to test. All tests are using a 3x3 board
testBoardSize :: Int
testBoardSize = 3

testBoardSpaces :: Int
testBoardSpaces = testBoardSize*testBoardSize

-- Empty stats used in the tests
testStats1 :: [(PlayerID, (Int, Int))]
testStats1 = [(PB,(0,0)), (PW,(0,0))]

-- Modified PB pass value used to check in updating players pass value
testStats2 :: [(PlayerID, (Int, Int))]
testStats2 = [(PB,(0,1)), (PW,(0,0))]

-- Modified PW pass value used to check in updating players pass value
testStats3 :: [(PlayerID, (Int, Int))]
testStats3 = [(PB,(0,0)), (PW,(0,1))]

-- A test board used to check a large number of functions
testBoard1 :: [(Char, (Int, Int))]
testBoard1 =
  [
    ('w',(0,0)),('w',(0,1)),('b',(0,2)),
    ('w',(0,3)),('b',(0,4)),('_',(0,5)),
    ('b',(0,6)),('_',(0,7)),('_',(0,8))
  ]

testBoard2 :: [(Char, (Int, Int))]
testBoard2 =
  [
    ('b',(0,0)),('b',(0,1)),('b',(0,2)),
    ('b',(0,3)),('w',(0,4)),('b',(0,5)),
    ('b',(0,6)),('w',(0,7)),('b',(0,8))
  ]

testBoard3 :: [(Char, (Int, Int))]
testBoard3 =
  [
    ('b',(0,0)),('b',(0,1)),('b',(0,2)),
    ('b',(0,3)),('_',(0,4)),('_',(0,5)),
    ('b',(0,6)),('_',(0,7)),('_',(0,8))
  ]

testBoard4 :: [(Char, (Int, Int))]
testBoard4 =
  [
    ('b',(0,0)),('b',(0,1)),('b',(0,2)),
    ('b',(0,3)),('_',(0,4)),('_',(0,5)),
    ('_',(0,6)),('w',(0,7)),('w',(0,8))
  ]


-- An empty board used to verify certain situations in tests
emptyBoardTest :: [(Char, (Int, Int))]
emptyBoardTest =
  [
    ('_',(0,0)),('_',(0,1)),('_',(0,2)),
    ('_',(0,3)),('_',(0,4)),('_',(0,5)),
    ('_',(0,6)),('_',(0,7)),('_',(0,8))
  ]


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
testEmptyBoard = "testEmptyBoard" ~: emptyBoard testBoardSize ~?= emptyBoardTest

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
        rowStates (rowLimit' 0) testBoard1 ~?= " w w b",
        rowStates (rowLimit' 1) testBoard1 ~?= " w b _",
        rowStates (rowLimit' 2) testBoard1 ~?= " b _ _"
      ]

      --        index  0 1 2
      -- players view  1 2 3
      --              _______  Positions avail from 0:
      --         0 1 | w w b |   0->2 
      --         1 2 | w b _ |   3->5
      --         2 3 | b _ _ |   6->8


testIdentifyUnits :: Test
testIdentifyUnits = "testIdentifyUnits" ~:
  identifyUnits' testBoard1 ~?= [[0,1,3]]





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


identifyUnits' :: Board -> [[Int]]
identifyUnits' []      = []
identifyUnits' (b:bs)  =
  do
    if curPID == '_' then [] : identifyUnits bs
    else do
      let findFriends =
            [
              (isSamePID' curPID bs north, north),
              (isSamePID' curPID bs south, south),
              (isSamePID' curPID bs east, east),
              (isSamePID' curPID bs west, west)
            ]
      let friends   = filter fst findFriends
      -- let friends'  = map snd friends : identifyUnits' bs
      let friends'  =
            if friends /= [] then
            (curPos : map snd friends) : identifyUnits' bs
            else [] : identifyUnits' bs
      let units = map sort . filter (not . null) . map nub $ friends'
      map sort . filter (not . null) . map nub $ uCombine [] units
      -- //FIXME not traversing the rest of the board it looks like...
      -- recursively call this function appending to the list generated by
      -- line 345 probably
      where
        curPos  = getPos b
        curPID  = fst b
        north   = curPos-testBoardSize
        south   = curPos+testBoardSize
        east    = curPos+1
        west    = curPos-1

-- The list of units identified regrouped into full lists of units based
-- on overlapping values.
-- //FIXME -- This might need to be [Int] -> [[Int]] -> [[Int]] instead...
uCombine :: [[Int]] -> [[Int]] -> [[Int]]
uCombine []     []      = []
uCombine (m:_)  []      = [m] `union` []
uCombine []     (x:xs)  = uCombine [[] `union` x] xs
uCombine m  [x]         = uCombine' m x
uCombine (m:ms) (x:xs)  | null m = do
                          let m' = m `union` x
                          m' : uCombine (m':ms) xs
                        | any (`elem` m) x  = do
                          let m' = m `union` x
                          uCombine (m':ms) xs
                        | otherwise = do
                          let ms' = ms `union` [x]
                          uCombine (m:ms'++ms) xs

uCombine' :: [[Int]] -> [Int] -> [[Int]]
uCombine' [] [] = []
uCombine' [] x  = [x]
uCombine' (m:ms) x  | any (`elem` m) x  = m `union` x : ms
                    | otherwise         = m : uCombine' ms x

--m' is the tracker. it must be passed along recursively, but the situation changes
-- if there's not a match found. Say m' doesn't have x... or xs... well then
  -- those should be unioned onto m' as a separate list?
  -- This probably needs another function to take the x and scan the m' list
  -- to see if it overlaps anywhere, if it does then union and that's it, otherwise
  -- keep looking. If no overlaps found, add it to m' as a whole separate group to
  -- keep an eye on.
  -- | (`elem` m) x = do
  --     let m' = sort (m `union` [x])
  --     m' `union` unitCombinator m' (xs:xss)
  -- | any (`elem` xs) x = do
  --     let m' = [sort (x `union` xs)]
  --     m' `union` unitCombinator m' xss
  -- | otherwise         = m `union` unitCombinator [x] (xs:xss)

-- [[0,1,3],[1,2],[2,3,5],[3,6],[4,7],[5,6,8]]
  --FINALLY A SOLUTION. MERGE ALL LISTS THAT HAVE OVERLAPPING VALUES!
  --YOU HAVE UNITS THEN!

-- :r
-- :break unitCombinator
-- :break 340
-- :break 341
-- :break 342
-- unitCombinator [] [[0,1,3],[1,2],[2,3,5],[3,6],[4,7],[5,6,8]]

-- m `union` [x `union` xs]







-- remaining :: [[Int]] -> [[Int]]
-- remaining [] = []
-- remaining (xs:xss) = let (ys, zs) = foldr merge ([], []) xss
--                       in if elem (head xs) ys || elem (head xs) zs then remaining xss
--                          else xs : remaining xss

-- merge :: [Int] -> ([Int], [Int]) -> ([Int], [Int])
-- merge [] (ys, zs) = (ys, zs)
-- merge (x:xs) (ys, zs)
--   | elem x ys = merge xs (x:ys, zs)
--   | elem x zs = merge xs (ys, x:zs)
--   | otherwise = merge xs (ys, zs)

isSamePID' :: Char -> Board -> Int -> Bool
isSamePID' _ [] _ = False
isSamePID' pID (b:bs) pos
  | pos < 0 || pos > testBoardSpaces  = False
  | pos /= getPos b                                 = isSamePID' pID bs pos
  | pos == getPos b && pID == getPID b              = True
  | otherwise                                       = False
