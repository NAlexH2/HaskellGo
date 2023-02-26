-- Alex Harris
-- Final Project: HaskellGo
-- Date Started: 2/25/2023

module HaskellGo (haskellGo, emptyBoard, boardSize) where
import Text.Printf ( printf )
import System.Process ( system )
import qualified GHC.IO.Exception


data Stones = Blank | Black | White | TBlack | TWhite deriving (Eq, Show)
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
haskellGo :: Board -> IO ()
haskellGo board = do
    _ <- clearScreen
    displayBoard board

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
    else printf "\n\n"




{-- TODO
  - function to identify if a user has changed the state in a position on the
    board.
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
