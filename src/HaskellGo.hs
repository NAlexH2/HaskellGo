{- 
Alex Harris
Final Project: HaskellGo
Date Started: 2/25/2023
-}

module HaskellGo (  haskellGo,
                    emptyBoard,
                    boardSize,
                    emptyStats,
                    notFirstPlayer,
                    newState ) where

import Text.Printf ( printf )
import System.Process ( system )
import System.Exit ( exitSuccess )
import System.IO ( hFlush )
import qualified GHC.IO.Exception
import GHC.IO.Handle.FD ( stdout )
import Data.Char ( toLower, isDigit )
import Data.List.Utils ( contains )


data Stones = Blank | Black | White | TBlack | TWhite deriving (Eq, Show)
data PlayerID = PB | PW deriving (Eq, Show) -- PB = PlayerBlack, PW = PlayerWhite
type PlayerStats = (PlayerID, (Int, Int))
-- First Int is score, 2nd is pass counter... This needs more tinkering
type Position = (Char, Int)
type Board = [Position]
type GameState = ([PlayerStats], Board)
-- playerStats = [(PB,(0,0)), (PW,(0,0))]

-- Define the size of the board here. Future would like to make it user choice
-- where n x n square and n >= 9
boardSize :: Int
boardSize = 9

-- specifically in refernce to the max size of boards list
boardSpaces :: Int
boardSpaces = (boardSize*boardSize)-1

rowSpaces :: Int
rowSpaces = boardSize - 1

-- Used to quickly identify if "pass" was entered
pass :: (Int, Int)
pass = (-99, -99)

-- Used to quickly identify if "quit" was entered
quit :: (Int, Int)
quit = (-100, -100)

-- Used to quickly check if there was an error on user input for getCoordinates
badInput :: (Int, Int)
badInput = (-1,-1)

-- Just some constructors for each char in the type position
stone :: Stones -> Char
stone Blank = '_'
stone Black = 'b'
stone TBlack = 'B'
stone White = 'w'
stone TWhite = 'W'

-- toggles between the users as the game goes
turnToggle :: PlayerID -> PlayerID
turnToggle PB = PW
turnToggle PW = PB

-- Identify the current player
currentPlayer :: PlayerID -> String
currentPlayer p | p == PB = "BLACK"
                | otherwise = "WHITE"

-- Return the player stone from Stones w/ respect to the current
-- playerID passed in
pStone :: PlayerID -> Char
pStone PB = stone Black
pStone PW = stone White


emptyStats :: [PlayerStats]
emptyStats = [(PB,(0,0)), (PW,(0,0))]

currentPlayersStats :: PlayerID -> [PlayerStats] -> PlayerStats
currentPlayersStats pID pStats = case filter (\(pid, _) -> pid == pID) pStats of
                            [] -> error "No Player Stats Available"
                            (x:_) -> x

-- Allows the code to swap between players. White initializes this
-- because the first call to `turnToggle` will have black go first.
notFirstPlayer :: PlayerID
notFirstPlayer = PW

statsState :: GameState -> [PlayerStats]
statsState = fst

boardState :: GameState -> Board
boardState = snd

newState :: [PlayerStats] -> Board -> GameState
newState a b = (a, b)



-- Simple utility function to clear the screen. At least on Linux type systems.
-- Helps players stay focused on the game versus every new output spamming
-- the screen.
clearScreen :: IO GHC.IO.Exception.ExitCode
clearScreen = system "clear"


-- This function utilizes the data Stones above. This data type will be
-- important later for identifying pieces on the board.
emptyBoard :: Int -> Board
emptyBoard n  =
  do
    let _ = clearScreen
    if n < 9 then []
    else [(stone Blank, i) | i <- [0..(n*n)-1]]
{- 
  /*FIXME - This is actually fine vs having a list of list of this. That is
  because we can calc East/West with (Start,End) - (s,e) - in the same I use it
  in rowLimit which takes (rowLimit row) before hand!
  -- Maybe could even use this in North/South?? //TODO?
-}

-- Where it all starts. Recursively runs the game using a do statement.
haskellGo :: GameState -> PlayerID -> IO ()
haskellGo currentGame pID =
  do
    -- //HACK - Remove these in final version
    -- print ((length board)-1)
    -- print (board !! 80)
    -- print (show board)
    printf "Type 'quit' anytime to end the game or 'pass' to skip your turn.\n"
    printf "To make your move, simply type an x and y that is on the grid. "
    printf "Like so:\n'x y: 9 9'\n\n"
    let pID' = turnToggle pID
    displayState currentGame
    printf "It is player %s's turn...\n" (currentPlayer pID')
    putStr "x y: "
    move <- getCoordinates
    -- //HACK - remove in final version
    -- print (show (statsState currentGame))
    -- uncurry (printf "\n\n\n(%d,%d) %d\n\n\n") move (posCalc move)
    let legality = legalMove (boardState currentGame) move
    if legality > 1 then do
      _ <- clearScreen
      -- //HACK - Remove these in final version
      -- uncurry (printf "\n\n\n(%d,%d)\n\n\n") move
      printf "\n**ERROR** - %s\n" (errorBadMove legality)
      let pID'' = turnToggle pID'
      haskellGo currentGame pID''
    else do
      -- //TODO see if player passed 2 times, end game if so.
      let stats' = updatePlayerPass pID' move (statsState currentGame)
      -- //HACK - Remove these in final version
      -- uncurry (printf "\n\n\n(%d,%d)\n\n\n") move
      let captured = capturedStones pID' currentGame
      let newStats = updateStats pID' stats' captured
      let newBoard = makeBoard pID' (boardState currentGame) (0, posCalc move)
      let currentGame' = updateGame newStats newBoard
      haskellGo currentGame' pID'

-- Creates a new GameState to be used for recursive play
updateGame :: [PlayerStats] -> Board -> GameState
updateGame s b = (s, b)

-- //TODO  -- Updates player stats for current pID if they captured stones
updateStats :: PlayerID -> [PlayerStats] -> [Int] -> [PlayerStats]
updateStats _ pStats [] = pStats
updateStats pId pStats (x:xs)= undefined


-- Quickly get the position in the Board[Position] list being changed
posCalc :: (Int, Int) -> Int
posCalc (x,y) = (x-1)+(y-1)*boardSize


-- make a move and new board based off of playeres coordinates
makeBoard :: PlayerID -> Board -> (Int, Int) -> Board
makeBoard _ [] (_, _)         = []
makeBoard pID (b:bs) (i, pos)
  | i == pos && snd b == pos  = (pStone pID, pos):makeBoard pID bs (i+1, pos)
  | i > boardSpaces           = []
  | otherwise                 = b:makeBoard pID bs (i+1, pos)


-- Checks to see if the user made a legal move on the current gameboard
legalMove :: Board -> (Int, Int) -> Int
legalMove board move  | move == pass            = 1
                      | move == badInput        = 2
                      | isOccupied board place  = 3
                      | place > boardSpaces     = 4
                      | otherwise               = 1
    where
      place = posCalc move

-- Error handling for the game. Allows user to correct mistakes
errorBadMove :: Int -> String
errorBadMove e  | e == 2      = "Received invalid input from the user."
                | e == 3      = "That space is occupied by the other player."
                | e == 4      = "This move is out of bounds for this board."
                | otherwise   = ""

-- Checks to see if the position passed in is occupied by another player
isOccupied :: Board -> Int -> Bool
isOccupied [] _                           = error "Empty game board detected"
isOccupied (b:bs) pos
  | pos > boardSpaces                     = False
  | pos == snd b && fst b /= stone Blank  = True
  | pos /= snd b                          = isOccupied bs pos
  | not $ null b && null bs               = False
  | otherwise                             = False

-- Identify the positions on the board which are to be "captured" when
-- building a new board.
capturedStones :: PlayerID -> GameState -> [Int]
capturedStones p game = undefined
  where
    b = boardState game
    s = statsState game


-- playerStats = [(PB,(0,0)), (PW,(0,0))]
updatePlayerPass :: PlayerID -> (Int, Int) -> [PlayerStats] -> [PlayerStats]
updatePlayerPass _ _ [p]        = [p]
updatePlayerPass _ _ []         = []
updatePlayerPass pID mv (p:ps)
  | pID /= fst p && mv == pass  = p:updatePlayerPass pID mv ps
  | otherwise                   = updatePlayerPass' p:ps

updatePlayerPass' :: PlayerStats -> PlayerStats
updatePlayerPass' ps = (fst ps, (fst (snd ps), snd (snd ps)+1))

-- Returns current players pass count
getPassCount :: PlayerID -> GameState -> Int
getPassCount pID game = snd $ snd pStats
  where
    pStats = currentPlayersStats pID (statsState game)



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


-- displays the current state of the game. 
-- Another do because of the fact each row itself
-- will be a recursive display. Both have to execute once here, while
-- while displayEachRow executes recursively
displayState :: GameState -> IO ()
displayState gameState =
  do
    displayTopRows
    displayEachRow row (boardState gameState)
    displayScore (statsState gameState)
      where
          row = 1

-- Visualization of the board to the user for x y coordinates to be entered
displayTopRows :: IO ()
displayTopRows = printf "  x  1 2 3 4 5 6 7 8 9\ny   __________________"

-- The most up-to date state of the board.
displayEachRow :: Int -> Board -> IO ()
displayEachRow row board =
    if row < boardSize+1 then
      do
        let x = show row ++ " | " ++ rowStates (rowLimit row) board
        printf "\n%s" x
        displayEachRow (row+1) board
    else printf "\n"

-- get the state of each position in each row
rowStates :: (Int,Int) -> Board -> String
rowStates _ []          = []
rowStates (s,e) (b:bs)
  | snd b < s           = rowStates (s,e) bs
  | s <= e              = " " ++ fst b : rowStates (s+1,e) bs
  | otherwise           =  [] -- rowStates (s+1,e) bs


-- This took a little to figure out. Each starting and ending index 
-- based on the row being passed in because the Board has n-Positions where
-- (n*n)-1 positions available. (start, end) for the current row
rowLimit :: Int -> (Int, Int)
rowLimit row = (row*10-(10+(row-1)), row*10-(row+1))


displayScore :: [PlayerStats] -> IO ()
displayScore stats =
  do
    printf "------------------------\n"
    printf "Score -- b: 0 -- w: 0\n\n"


{-- /*TODO
   function to identify if a user has changed the state in a position on the
    board.
  [ ] See about checking North, South, East and West liberties using the
      rowLimit. Write an singular function for each of these
      - names: check____ || is____ ||
      [ ] Need a function isOccupied :: Board -> (x,y) -> Bool
          - uses posCalc for (x,y) probably
  [ ] Use a GameState versus seperate vars to track the ENTIRE game
  [ ] Check if move is valid
  [ ] functions to look north, south, east and west on the board. bool returns?
  [ ] Check and capture if liberties are gone
  [ ] Player stats -- kinda has to be done in tandem with capture
  [ ] tbd
  [x] Proper state and display
  [x] Player input
  [x] Player toggler
  */
 --}
