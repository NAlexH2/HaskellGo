module GoWork where

import GoTypesData
import GoConsts
import System.Process ( system )
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
currentPlayer p | p == PB   = "BLACK"
                | otherwise = "WHITE"


-- Return the player stone from Stones w/ respect to the current playerID
pStone :: PlayerID -> Char
pStone PB = stone Black
pStone PW = stone White


-- Get the current PlayerStats from the GameState
statsState :: GameState -> [PlayerStats]
statsState = fst


-- Get the current Board for the game
boardState :: GameState -> Board
boardState = snd


-- Create a new GameState with (typically) updated information.
newState :: [PlayerStats] -> Board -> GameState
newState a b = (a, b)


-- This function utilizes the data Stones above. This data type will be
-- important later for identifying pieces on the board.
emptyBoard :: Int -> Board
emptyBoard bS =
  do
    let _ = clearScreen
    [(stone Blank, (r,i)) | i <- [0..(bS*bS)-1], let r = currentRow bS i]


-- The following are cardinal positions on the board from 
-- position value passed in
north :: Int -> Int -> Int
north pos bdSz = pos - bdSz

south :: Int -> Int -> Int
south pos bdSz = pos + bdSz

east :: Int -> Int
east pos = pos + 1

west :: Int -> Int
west pos = pos - 1


-- Return the current rows number for the position 'i' that was provided.
currentRow :: Int -> Int  -> Int
currentRow bdSz i = i `div` bdSz


-- Return the previous rows number for the position 'i' that was provided
previousRow :: Int ->  Int -> Int
previousRow bdSz i = (i `div` bdSz)-1


-- Return the next rows number for the position 'i' that was provided
nextRow :: Int ->  Int -> Int
nextRow bdSz i = (i `div` bdSz)+1


-- Access the current position from the position passed in.
getPos :: Position -> Int
getPos pos = snd (snd pos)


-- Return the PlayerID as a Char from the current position.
getPID :: Position -> Char
getPID = fst


-- Calculates start and end positions of the row passed in
-- with the boardSize too
rowLimit :: Int -> Int -> (Int, Int)
rowLimit bdSz row = (row*bdSz, row*bdSz+(bdSz-1))


-- Creates a new GameState to be used for recursive play
updateGame :: [PlayerStats] -> Board -> GameState
updateGame s b = (s, b)


-- Return the PlayerID stats passed in.
thisPlayersStats :: PlayerID -> [PlayerStats] -> PlayerStats
thisPlayersStats _ [p]      = p
thisPlayersStats _ []       = error "No Stats Found"
thisPlayersStats pID (p:ps)
  | pID == fst p            = p
  | otherwise               = thisPlayersStats pID ps


-- Performs pattern matching to ensure we update the correct players pass stat
-- Resets current players pass counter if the move wasn't identified as a
-- pass.
updatePlayerPass :: PlayerID -> (Int, Int) -> [PlayerStats] -> [PlayerStats]
updatePlayerPass _ _ []         = []
updatePlayerPass pID mv (p:ps)
  | pID /= fst p                = p:updatePlayerPass pID mv ps
  | pID == fst p && mv /= pass  = resetPlayerPass pID p:ps
  | otherwise                   = updatePlayerPass' pID p:ps


-- Creates new PlayerStats for the correct player incrementing their pass
updatePlayerPass' :: PlayerID -> PlayerStats -> PlayerStats
updatePlayerPass' pID p = (pID, (score, passC+1))
  where
    score = fst $ snd p
    passC = snd $ snd p


-- Reset the passed in PlayerID's pass count
resetPlayerPass :: PlayerID -> PlayerStats -> PlayerStats
resetPlayerPass pID p = (pID, (score, 0))
  where
    score = fst $ snd p


-- Updates player stats for current pID if they captured stones
updateStats :: PlayerID -> [PlayerStats] -> [Int] -> [PlayerStats]
updateStats _ pStats []     = pStats
updateStats _ [] _          = []
updateStats pID (p:ps) caps
  | pID /= fst p            = p:updateStats pID ps caps
  | otherwise               = updateStats' lenCaps pID p:ps
  where
    lenCaps = length caps

updateStats' :: Int -> PlayerID -> PlayerStats -> PlayerStats
updateStats' i pID p = (pID, (score+i, passC))
  where
    score = fst $ snd p
    passC = snd $ snd p


-- Returns the provided PlayerID current score
getPlayerScore :: PlayerID -> [PlayerStats] -> Int
getPlayerScore pID stats = fst $ snd pStats
  where
    pStats = thisPlayersStats pID stats


-- Returns the provided PlayerID pass count
getPassCount :: PlayerID -> [PlayerStats] -> Int
getPassCount pID stats = snd $ snd pStats
  where
    pStats = thisPlayersStats pID stats


-- Quickly get the position in the Board[Position] list being changed
-- based on the x,y coordinates the user entered (-1 because of list indexing)
posCalc :: (Int, Int) -> Int -> Int
posCalc (x,y) bdSz = (x-1)+((y-1)*bdSz)


-- Make a move and new board based off of players coordinates
-- i is current pos
makeBoard :: Int -> PlayerID -> Board -> Int -> [Int] -> Board
makeBoard _ _ [] _ _  = []
makeBoard bdSz pID (b:bs) pos caps
  | cPos `elem` caps  = posUpdate bdSz '_' cPos:makeBoard bdSz pID bs pos caps
  | sPos              = posUpdate bdSz iPID pos:makeBoard bdSz pID bs pos caps
  | otherwise         = b:makeBoard bdSz pID bs pos caps
  where
    cPos = getPos b
    sPos = cPos == pos
    iPID = pStone pID


-- Return a new position with the passed in PlayerID
posUpdate :: Int -> Char -> Int -> Position
posUpdate bdSz cID pos = (cID, (currentRow bdSz pos, pos))


-- Checks to see if the user made a legal move on the current game board
legalMove :: Int -> Board -> (Int, Int) -> Int
legalMove bdSz board move
  | move == pass                = 1
  | move == badInput            = 2
  | isOccupied bdSz board place = 3
  | place > boardSpaces bdSz    = 4
  | otherwise                   = 1
    where
      place = posCalc move bdSz


-- Checks to see if the position passed in is occupied by another player
isOccupied :: Int -> Board -> Int -> Bool
isOccupied _ [] _                           = False
isOccupied bdSz (b:bs) pos
  | pos > boardSpaces bdSz                  = False
  | pos /= getPos b                         = isOccupied bdSz bs pos
  | pos == getPos b && fst b /= stone Blank = True
  | not $ null b && null bs                 = False
  | otherwise                               = False


-- Obtain user input coordinates in a x y format to be used to place a stone
-- down on the board.
getCoordinates :: IO (Int, Int)
getCoordinates =
  do
    hFlush stdout
    coords <- getLine
    if contains "pass" $ map toLower coords 
      then return (-99,-99)

    else if contains "quit" $ map toLower coords 
      then return (-100,-100)

    else if contains "-" coords 
      then return (-1, -1)

    else do
      -- Higher order! Found a place!
      let coords'   = filter (not . null) $ map (filter isDigit) (words coords)
      if length coords' /= 2 
        then return (-1, -1)
        
      else do
        let (x, y)  = (\b -> (read (head b) ::Int, read (last b) ::Int)) coords'
        return (x,y)


-- Get the state of each position in a row as a string
rowStates :: (Int,Int) -> Board -> String
rowStates _ []          = []
rowStates (s,e) (b:bs)
  | getPos b < s        = rowStates (s,e) bs
  | s <= e              = " " ++ fst b : rowStates (s+1,e) bs
  | otherwise           = []