module GoWork where

import GoTypesData
    ( GameState,
      Board,
      PlayerStats,
      PlayerID(..),
      stone,
      Stones(Blank, Black, White), Position )
import GoConsts ( boardSize, boardSpaces, pass, badInput )
import System.Process ( system )
import System.Exit ( exitSuccess )
import System.IO ( hFlush )
import qualified GHC.IO.Exception
import GHC.IO.Handle.FD ( stdout )
import Data.List.Utils ( contains )
import Data.Char ( toLower, isDigit )


-- Simple utility function to clear the screen. At least on Linux type systems.
-- Helps players stay focused on the game versus every new output spamming
-- the screen.
clearScreen :: IO GHC.IO.Exception.ExitCode
clearScreen = system "clear"


-- toggles between the users as the game goes
turnToggle :: PlayerID -> PlayerID
turnToggle PB = PW
turnToggle PW = PB


-- Identify the current player
currentPlayer :: PlayerID -> String
currentPlayer p | p == PB = "BLACK"
                | otherwise = "WHITE"


-- Return the player stone from Stones w/ respect to the current playerID
pStone :: PlayerID -> Char
pStone PB = stone Black
pStone PW = stone White


-- //TODO - Comment
currentPlayersStats :: PlayerID -> [PlayerStats] -> PlayerStats
currentPlayersStats pID pStats = case filter (\(pid, _) -> pid == pID) pStats of
                            [] -> error "No Player Stats Available"
                            (x:_) -> x

-- //TODO - Comment
statsState :: GameState -> [PlayerStats]
statsState = fst


-- //TODO - Comment
boardState :: GameState -> Board
boardState = snd


-- //TODO - Comment
newState :: [PlayerStats] -> Board -> GameState
newState a b = (a, b)


-- This function utilizes the data Stones above. This data type will be
-- important later for identifying pieces on the board.
emptyBoard :: Int -> Board
emptyBoard n  =
  do
    let _ = clearScreen
    [(stone Blank, (r,i)) | i <- [0..(n*n)-1], let r = currentRow i]
    -- //TODO if i/boardsize /= whole number then r+1 else r


-- //TODO - Comment
currentRow :: Int -> Int
currentRow i = i `div` boardSize

-- //TODO - Comment
previousRow :: Int -> Int
previousRow i = (i `div` boardSize)-1

-- //TODO - Comment
nextRow :: Int -> Int
nextRow i = (i `div` boardSize)+1

-- //TODO - Comment
getPos :: Position -> Int
getPos pos = snd (snd pos)

-- Calculates start and end positions of the row passed in
-- with the given boardSize provided at the top in GoConsts.hs
rowLimit :: Int -> (Int, Int)
rowLimit row = (row*boardSize, row*boardSize+(boardSize-1))

{- 
  /*FIXME - This is actually fine vs having a list of list of this. That is
  because we can calc East/West with (Start,End) - (s,e) - in the same I use it
  in rowLimit which takes (rowLimit row) before hand!
  -- Maybe could even use this in North/South?? //TODO?
-}

-- Creates a new GameState to be used for recursive play
updateGame :: [PlayerStats] -> Board -> GameState
updateGame s b = (s, b)


-- //TODO  -- Updates player stats for current pID if they captured stones
updateStats :: PlayerID -> [PlayerStats] -> [Int] -> [PlayerStats]
updateStats _ pStats [] = pStats
updateStats pId pStats (x:xs) = undefined


-- Quickly get the position in the Board[Position] list being changed
posCalc :: (Int, Int) -> Int
posCalc (x,y) = (x-1)+(y-1)*boardSize


-- make a move and new board based off of playeres coordinates
-- //TODO holy cow fix this mess... i == pos && snd snd b == .... alllll that.
-- Make it easy to read dude
makeBoard :: PlayerID -> Board -> (Int, Int) -> Board
makeBoard _ [] (_, _)           = []
makeBoard pID (b:bs) (i, pos)
  | i == pos && getPos b == pos = posUpdate pID pos:makeBoard pID bs (i+1, pos)
  | i > boardSpaces             = []
  | otherwise                   = b:makeBoard pID bs (i+1, pos)

-- //TODO - Comment
posUpdate :: PlayerID -> Int -> Position
posUpdate pID pos = (pStone pID, (currentRow pos, pos))



-- Checks to see if the user made a legal move on the current gameboard
legalMove :: Board -> (Int, Int) -> Int
legalMove board move  | move == pass            = 1
                      | move == badInput        = 2
                      | isOccupied board place  = 3
                      | place > boardSpaces     = 4
                      | otherwise               = 1
    where
      place = posCalc move


-- Checks to see if the position passed in is occupied by another player
isOccupied :: Board -> Int -> Bool
isOccupied [] _                             = error "Empty game board detected"
isOccupied (b:bs) pos
  | pos > boardSpaces                       = False
  | pos == getPos b && fst b /= stone Blank = True
  | pos /= getPos b                         = isOccupied bs pos
  | not $ null b && null bs                 = False
  | otherwise                               = False


-- Identify the positions on the board which are to be "captured" when
-- building a new board.
capturedStones :: PlayerID -> GameState -> [Int]
capturedStones p game = undefined
  where
    b = boardState game
    s = statsState game

-- check the current position on the board to see if is to be considered
-- captured. 
checkLiberties :: Board -> Int -> Bool
checkLiberties board pos = and bools
  where
    north = pos-9
    south = pos+9
    east = pos+1
    west = pos-1
    bools =
      [
        occupiedNorth board north (rowLimit (previousRow pos)),
        occupiedSouth board south (rowLimit (nextRow pos)),
        occupiedEast board east (rowLimit (currentRow pos)),
        occupiedWest board west (rowLimit (currentRow pos))
      ]


-- Check the respective cardinals for the current position on the board for
-- each of the following functions. If occupied then TRUE, otherwise FALSE.
-- If it is occupied, that liberty is no longer available. This is important
-- for determining captured stones.
occupiedNorth :: Board -> Int -> (Int, Int) -> Bool
occupiedNorth board pos' (s, _) | s < boardSpaces       = True
                                | isOccupied board pos' = True
                                | otherwise             = False

occupiedSouth :: Board -> Int -> (Int, Int) -> Bool
occupiedSouth board pos' (_, e) | e > boardSpaces       = True
                                | isOccupied board pos' = True
                                | otherwise             = False

occupiedEast :: Board -> Int -> (Int, Int) -> Bool
occupiedEast board pos' (_, e)  | pos' > e              = True
                                | isOccupied board pos' = True
                                | otherwise             = False

occupiedWest :: Board -> Int -> (Int, Int) -> Bool
occupiedWest board pos' (s, _)  | pos' < s              = True
                                | isOccupied board pos' = True
                                | otherwise             = False


-- Performs pattern matching to ensure we update the correct players pass stat
updatePlayerPass :: PlayerID -> (Int, Int) -> [PlayerStats] -> [PlayerStats]
updatePlayerPass _ _ [p]        = [p]
updatePlayerPass _ _ []         = []
updatePlayerPass pID mv (p:ps)
  | pID /= fst p && mv == pass  = p:updatePlayerPass pID mv ps
  | otherwise                   = updatePlayerPass' p:ps

-- Creates new PlayerStats for the correct player incrementing their pass
updatePlayerPass' :: PlayerStats -> PlayerStats
updatePlayerPass' ps = (fst ps, (fst (snd ps), snd (snd ps)+1))


-- Returns current players pass count
getPassCount :: PlayerID -> GameState -> Int
getPassCount pID game = snd $ snd pStats
  where
    pStats = currentPlayersStats pID (statsState game)


-- //TODO - Comment
getCoordinates :: IO (Int, Int)
getCoordinates =
  do
    hFlush stdout
    coords <- getLine
    -- //TODO - implement pass once stats is working. Should be easy... I hope
    if contains "pass" $ map toLower coords then return (-99,-99)
    else if contains "quit" $ map toLower coords then exitSuccess
    else if contains "-" coords then return (-1, -1)
    else do
      -- Higher order! Found a place!
      let coords'   = filter (not . null) $ map (filter isDigit) (words coords)
      if length coords' /= 2 then return (-1, -1)
      else do
        let (x, y)  = (\b -> (read (head b) ::Int, read (last b) ::Int)) coords'
        return (x,y)


-- Get the state of each position in a row as a string
rowStates :: (Int,Int) -> Board -> String
rowStates _ []          = []
rowStates (s,e) (b:bs)
  | getPos b < s        = rowStates (s,e) bs
  | s <= e              = " " ++ fst b : rowStates (s+1,e) bs
  | otherwise           =  [] -- rowStates (s+1,e) bs //TODO What is this comment for?


