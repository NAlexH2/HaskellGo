module GoCapture where

import GoTypesData
import GoWork
import GoConsts
import Data.List ( union, nub, sort )


-- Returns a list of all stones - both singles and units - to be removed
-- from the board and added to the score of the player who captured.
-- The PlayerID is the player NOT CURRENTLY TAKING THEIR TURN
capturedStones :: Int -> PlayerID -> Board -> [Int]
capturedStones bdSz pID' board =
  do
    let units     = identifyUnits bdSz board
    let cdSingles = cappedSingles bdSz units pID' board board
    let cdUnits   = cappedUnits bdSz units pID' board board
    cdSingles ++ cdUnits


-- Analyzes a board for any single stones that might be captured.
-- Takes... board size, unit of stones, a PlayerID and two boards:
-- one to traverse and compare and one to reference when examined in other
-- functions against the current position.
cappedSingles :: Int -> [[Int]] -> PlayerID -> Board -> Board -> [Int]
cappedSingles _ _ _ [] _  = []
cappedSingles bdSz units pID (b:bs) ref
  | inUnits               = cappedSingles bdSz units pID bs ref
  | sPID && libCheck      = pos : cappedSingles bdSz units pID bs ref
  | otherwise             = cappedSingles bdSz units pID bs ref
  where
    pos       = getPos b
    inUnits   = any (pos `elem`) units
    sPID      = getPID b == pStone pID
    libCheck  = lostLiberties bdSz ref pos


-- Identifies capped units for the current board. Capped units means every
-- single stone in the unit has lost all its liberties
cappedUnits :: Int -> [[Int]] -> PlayerID -> Board -> Board -> [Int]
cappedUnits _ [] _ _ _  = []
cappedUnits bdSz (u:us) pID board ref =
  cappedUnits' bdSz u pID board ref ++ cappedUnits bdSz us pID board ref

cappedUnits' :: Int-> [Int] -> PlayerID -> Board -> Board -> [Int]
cappedUnits' _ [] _ _ _  = []
cappedUnits' _ _ _ [] _  = []
cappedUnits' bdSz (u':us') pID (b:bs) ref
  | not uPID    = []
  | not sPID    = cappedUnits' bdSz (u':us') pID bs ref
  | allLiberties && sPID = u':us'
  | otherwise = cappedUnits' bdSz (u':us') pID bs ref
  where
    thisPID = pStone pID
    cPos = getPos b
    uPID = isSamePID bdSz thisPID ref u'
    sPID = isSamePID bdSz thisPID ref cPos
    allLiberties = and [r | z <- u':us', let r = lostLiberties bdSz ref z]

-- check the current position on the board to see if is to be considered
-- captured. If all bools are true (all liberties are lost) then return True, 
-- otherwise return false False
lostLiberties :: Int -> Board -> Int -> Bool
lostLiberties bdSz board pos = and bools
  where
    n   = north pos bdSz
    s   = south pos bdSz
    e    = east pos
    w    = west pos
    nLimits = rowLimit bdSz (previousRow bdSz pos)
    sLimits = rowLimit bdSz (nextRow bdSz pos)
    e_wLimits = rowLimit bdSz (currentRow bdSz pos)
    bools =
      [
        occupiedNorth bdSz board n nLimits,
        occupiedSouth bdSz board s sLimits,
        occupiedEast bdSz board e e_wLimits,
        occupiedWest bdSz board w e_wLimits
      ]


-- Check the respective cardinals for the current position on the board for
-- each of the following functions. If occupied then TRUE, otherwise FALSE.
-- If it is occupied, that liberty is no longer available. This is important
-- for determining captured stones.
occupiedNorth :: Int -> Board -> Int -> (Int, Int) -> Bool
occupiedNorth bdSz board pos' (s, _)  | s < boardSpaces bdSz        = True
                                      | isOccupied bdSz board pos'  = True
                                      | otherwise                   = False

occupiedSouth :: Int -> Board -> Int -> (Int, Int) -> Bool
occupiedSouth bdSz board pos' (_, e)  | e > boardSpaces bdSz        = True
                                      | isOccupied bdSz board pos'  = True
                                      | otherwise                   = False

occupiedEast :: Int -> Board -> Int -> (Int, Int) -> Bool
occupiedEast bdSz board pos' (_, e)   | pos' > e                    = True
                                      | isOccupied bdSz board pos'  = True
                                      | otherwise                   = False

occupiedWest :: Int -> Board -> Int -> (Int, Int) -> Bool
occupiedWest bdSz board pos' (s, _)   | pos' < s                    = True
                                      | isOccupied bdSz board pos'  = True
                                      | otherwise                   = False


-- Units in Go are all stones connected to one another as long as they are
-- of matching color. This function correctly identifies those units and returns
-- that as a [[Int]] to be used later to verify whether or not that set has
-- lost all their liberties and must be captured.
identifyUnits :: Int -> Board -> [[Int]]
identifyUnits _ []        = []
identifyUnits bdSz (b:bs) =
  do
    -- Ignore blank spaces on board
    if curPID == '_' then [] : identifyUnits bdSz bs 
    else do
      let findFriends = -- Find all the adjacent positions w/ same pID
            [
              (isSamePID bdSz curPID bs n, n),
              (isSamePID bdSz curPID bs s, s),
              (isSamePID bdSz curPID bs e, e),
              (isSamePID bdSz curPID bs w, w)
            ]

      -- Pull out only the values from the list where the first of the tuple was
      -- True
      let friends   = filter fst findFriends
      let friends'  = 
            if friends /= [] then
              (curPos : map snd friends) : identifyUnits bdSz bs
            else [] : identifyUnits bdSz bs
      -- For friends', if the list is not [], take the current position and
      -- build a list of all the friends we found with it. Then recursively
      -- call this function and add more friends to the list of lists this is
      -- building.
            
      let units = uFilter friends' -- Get rid of duplicates, empties, and sort
      uFilter (uCombine [] units) -- Combine all friends by identifying
      -- overlapping values in the lists. Then, once again, clean up and sort.
      -- [[0,1,3],[2,3],[3,5,6],[4,7],[5,8]] 
      -- [[0,1,2,3,5,6,8],[4,7]]

  where
    curPos  = getPos b
    curPID  = fst b
    n   = north curPos bdSz
    s   = south curPos bdSz
    e    = east curPos
    w    = west curPos


-- The list of units identified regrouped into full lists of units based
-- on overlapping values.
uCombine :: [[Int]] -> [[Int]] -> [[Int]]
uCombine []     []      = []
uCombine (m:_)  []      = [m] `union` []
uCombine m  [x]         = uCombine' m x
uCombine []     (x:xs)  = uCombine [[] `union` x] xs
uCombine (m:ms) (x:xs)  | null m            = m' : uCombine (m':ms) xs
                        | any (`elem` m) x  = uCombine (m':ms) xs
                        | otherwise         = uCombine (m:ms' ++ ms) xs
  where
    m' = m `union` x
    ms' = ms `union` [x]


-- Checks the last element (x) in the list against the currently existing
-- merged list to see if it belongs anywhere. If it does, union it with that
-- list. Otherwise, recurse until either it does or it exists on its own.
uCombine' :: [[Int]] -> [Int] -> [[Int]]
uCombine' [] []     = []
uCombine' [] x      = [x]
uCombine' (m:ms) x  | any (`elem` m) x  = m `union` x : ms
                    | otherwise         = m : uCombine' ms x


-- For list of int lists passed in (read from right to left): 
-- remove dupes, filter non empty/null, then finally sort it.
uFilter :: [[Int]] -> [[Int]]
uFilter = map sort . filter (not . null) . map nub


-- Check to see if current element of the board's state/player matches the
-- player id that was passed in. Used in identifyUnits
isSamePID :: Int -> Char -> Board -> Int -> Bool
isSamePID _ _ [] _                      = False
isSamePID bdSz pID (b:bs) pos
  | pos < 0 || pos > boardSpaces bdSz   = False
  | not posCheck                        = isSamePID bdSz pID bs pos
  | posCheck && pIDCheck                = True
  | otherwise                           = False
  where
    posCheck = pos == getPos b
    pIDCheck = pID == getPID b

