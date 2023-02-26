-- Alex Harris
-- Final Project: HaskellGo
-- Date Started: 2/25/2023

module HaskellGo (haskellGo, emptyBoard, boardSize, playerID) where
import Text.Printf ( printf )
import System.Process ( system )
import qualified GHC.IO.Exception


data Stones = Blank | Black | White | TBlack | TWhite deriving (Eq, Show)
data Player = PB | PW deriving (Eq, Show) -- PB = PlayerBlack, PW = PlayerWhite
type PlayerStats = (Player, (Int, Int))
-- First Int is score, 2nd is pass counter... This needs more tinkering
type Position = (Char, Int)
type Board = [Position]

-- Define the size of the board here. Future would like to make it user choice
-- where n x n square and n >= 9
boardSize :: Int
boardSize = 9

-- Just some constructors for each char in the type position
stone :: Stones -> Char
stone Blank = '_'
stone Black = 'b'
stone TBlack = 'B'
stone White = 'w'
stone TWhite = 'W'

-- toggles between the users as the game goes
turnToggle :: Player -> Player
turnToggle PB = PW
turnToggle PW = PB

currentPlayer :: Player -> String
currentPlayer p | p == PB = "BLACK"
                | otherwise = "WHITE"


playerStats = [(PB,(0,0)), (PW,(0,0))]

-- Allows the code to swap between players. White initializes this
-- because the first call to `turnToggle` will have black go first.
playerID = PW


-- Simple utility function to clear the screen. At least on Linux type systems.
-- Helps players stay focused on the game versus every new output spamming
-- the screen.
clearScreen :: IO GHC.IO.Exception.ExitCode
clearScreen = system "clear"


-- This function utilizes the data Stones above. This data type will be
-- important later for identifying pieces on the board.
emptyBoard :: Int -> Board
emptyBoard n  | n < 9     = []
              | otherwise = [(stone Blank, i) | i <- [1..n*n]]


-- Where it all starts. Recursively runs the game using a do statement.
haskellGo :: Board -> Player -> IO ()
haskellGo board playerID = 
  do
    _ <- clearScreen
    printf "Type 'quit' anytime to end the game or 'pass' to skip your turn.\n"
    printf "To make your move, simply type an x and y that is on the grid. "
    printf "Like so:\n 'HaskellGo > 9 9'\n"
    let playerID' = turnToggle playerID
    displayBoard board
    displayScore playerStats
    printf "It is %s's turn: " currentPlayer
    -- how to get user input into tuple? ask for x then y seems best but sloppy
    let newBoard = makeMove playerID' board move'
    haskellGo newBoard playerID'

makeMove :: Player -> Board -> (Int,Int) -> Board
makeMove pID board = undefined



-- displays the current board. Another do because of the fact each row itself
-- will be a recursive display. Both have to execute once here, while
-- while displayEachRow executes recursively
displayBoard :: Board -> IO ()
displayBoard board =
  do
    displayTopRows
    displayEachRow row board
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
        let x = show row ++ " | " ++ concat (replicate boardSize " _")
        printf "\n%s" x
        displayEachRow (row+1) board
    else printf "\n"


displayScore :: [PlayerStats] -> IO ()
displayScore stats = 
  do
    printf "------------------------\n"
    printf "Score -- b: 0 -- w: 0\n\n"


{-- TODO
  - function to identify if a user has changed the state in a position on the
    board.
  - Player toggler
  - Player input
  - Player stats
  - functions to go north, south, east and west on the board.
  - tbd
 --}



{-- ASIDES: my other thoughts

-- boardPlace :: Char -> (Int, Int) -> [Char, Int, Int]
-- boardPlace a b  | a /= '_'  = a:uncurry b
--                 | otherwise = Nothing 

-- type probably best to start with then consider other options
-- eg: [[Char, Int]] where int represents the position on the board
-- by way of calculating user input --> 
-- example: b 9 9 = (9-1)*(9-1) = 64, go to that
-- subtract 1 because array is only 0->8 on 9x9 board.
-- position on the board doing linear search (ew)

--}
