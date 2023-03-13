module GoWork where

import GoTypesData
import GoConsts
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


-- Return the PlayerID stats passed in.
currentPlayersStats :: PlayerID -> [PlayerStats] -> PlayerStats
currentPlayersStats pID pStats = case filter (\(pid, _) -> pid == pID) pStats of
                            [] -> error "No Player Stats Available"
                            (x:_) -> x


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

-- Get the next position in the list
-- //TODO is this actually used?
getNext :: Int -> Board -> Position
getNext _ [] = ('d',(-1,-1))
getNext pos (b:bs)  | (pos+1) /= getPos b = getNext pos bs
                    | otherwise = b

-- Access the current position from the position passed in.
getPos :: Position -> Int
getPos pos = snd (snd pos)


-- Return the PlayerID as a Char from the current position.
getPID :: Position -> Char
getPID = fst

-- //TODO - Do you need this?
-- safeHead :: [Int] -> Maybe Int
-- safeHead [] = Nothing
-- safeHead (x:_) = Just x

-- Calculates start and end positions of the row passed in
-- with the boardSize too
rowLimit :: Int -> Int -> (Int, Int)
rowLimit bdSz row = (row*bdSz, row*bdSz+(bdSz-1))


-- Creates a new GameState to be used for recursive play
updateGame :: [PlayerStats] -> Board -> GameState
updateGame s b = (s, b)


-- //TODO  -- Updates player stats for current pID if they captured stones
updateStats :: PlayerID -> [PlayerStats] -> [Int] -> [PlayerStats]
updateStats _ pStats [] = pStats
updateStats pId pStats (x:xs) = undefined


-- Quickly get the position in the Board[Position] list being changed
-- based on the x,y coordinates the user entered (-1 because of list indexing)
posCalc :: (Int, Int) -> Int -> Int
posCalc (x,y) bdSz = (x-1)+((y-1)*bdSz)


-- Make a move and new board based off of players coordinates
makeBoard :: Int -> PlayerID -> Board -> (Int, Int) -> Board
makeBoard _ _ [] (_, _)           = []
makeBoard bdSz pID (b:bs) (i, pos)
  | i == pos && getPos b == pos =
      posUpdate bdSz pID pos:makeBoard bdSz pID bs (i+1, pos)
  | i > boardSpaces bdSz   = []
  | otherwise                   = b:makeBoard bdSz pID bs (i+1, pos)


-- Return a new position with the passed in PlayerID
posUpdate :: Int -> PlayerID -> Int -> Position
posUpdate bdSz pID pos = (pStone pID, (currentRow bdSz pos, pos))


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

  -- //TODO what if 151 looked to check opposite pID and not blank?
  -- if they are false and true respectively, return true, else false


-- Performs pattern matching to ensure we update the correct players pass stat
updatePlayerPass :: PlayerID -> (Int, Int) -> [PlayerStats] -> [PlayerStats]
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


-- Obtain user input coordinates in a x y format to be used to place a stone
-- down on the board.
getCoordinates :: IO (Int, Int)
getCoordinates =
  do
    hFlush stdout
    coords <- getLine
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
  | otherwise           =  []