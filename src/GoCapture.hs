module GoCapture where

import GoTypesData
import GoWork
import GoConsts
import Data.List ( union, nub, sort )

-- This is the start of the capture process. It will check single stones, units
-- and do this for both black and white stones. Everyone gets looked at and
-- assessed.
theCaptureCode :: Int -> Board -> [Int]
theCaptureCode bdSz board = undefined


-- Identify the positions on the board which are to be "captured" when
-- building a new board.
-- //TODO - finish this up when you've decided on the approach to identify...
--          Literally everything it feels like. Reference your notes in the
--          TODO in HaskellGo.hs
capturedStones :: PlayerID -> GameState -> [Int]
capturedStones p game = undefined
  where
    b = boardState game
    s = statsState game

-- check the current position on the board to see if is to be considered
-- captured. 
checkLiberties :: Int -> Board -> Int -> Bool
checkLiberties bdSz board pos = and bools
  where
    north = pos-bdSz
    south = pos+bdSz
    east  = pos+1
    west  = pos-1
    bools =
      [
        occupiedNorth bdSz board north (rowLimit bdSz (previousRow bdSz pos)),
        occupiedSouth bdSz board south (rowLimit bdSz (nextRow bdSz pos)),
        occupiedEast bdSz board east (rowLimit bdSz (currentRow bdSz pos)),
        occupiedWest bdSz board west (rowLimit bdSz (currentRow bdSz pos))
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
occupiedEast bdSz board pos' (_, e) | pos' > e              = True
                                    | isOccupied bdSz board pos' = True
                                    | otherwise             = False

occupiedWest :: Int -> Board -> Int -> (Int, Int) -> Bool
occupiedWest bdSz board pos' (s, _) | pos' < s              = True
                                    | isOccupied bdSz board pos' = True
                                    | otherwise             = False


-- 
identifyUnits :: Int -> Board -> [[Int]]
identifyUnits _ []        = []
identifyUnits bdSz (b:bs) =
  do
    if curPID == '_' then [] : identifyUnits bdSz bs
    else do
      let findFriends =
            [
              (isSamePID bdSz curPID bs north, north),
              (isSamePID bdSz curPID bs south, south),
              (isSamePID bdSz curPID bs east, east),
              (isSamePID bdSz curPID bs west, west)
            ]
      let friends   = filter fst findFriends
      -- let friends'  = map snd friends : identifyUnits' bs
      let friends'  =
            if friends /= [] then
            (curPos : map snd friends) : identifyUnits bdSz bs
            else [] : identifyUnits bdSz bs
      let units = uFilter friends'
      uFilter (uCombine [] units)

      where
        curPos  = getPos b
        curPID  = fst b
        north   = curPos-bdSz
        south   = curPos+bdSz
        east    = curPos+1
        west    = curPos-1


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
uCombine' [] [] = []
uCombine' [] x  = [x]
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
  | pos /= getPos b                     = isSamePID bdSz pID bs pos
  | pos == getPos b && pID == getPID b  = True
  | otherwise                           = False



identifySingles :: PlayerID -> Board -> [Int]
identifySingles pID board = undefined