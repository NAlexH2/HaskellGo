-- Alex Harris
-- Final Project: HaskellGo
-- Date Started: 2/25/2023

module HaskellGo (  haskellGo,
                    emptyBoard,
                    boardSize,
                    playerID,
                    playerStats,
                    GameState ) where
import Text.Printf ( printf )
import System.Process ( system )
import System.Exit ( exitSuccess )
import System.IO ( hFlush )
import qualified GHC.IO.Exception
import GHC.IO.Handle.FD ( stdout )
import Data.Char ( toLower, isDigit )
import Data.List.Utils ( contains )


data Stones = Blank | Black | White | TBlack | TWhite deriving (Eq, Show)
data Player = PB | PW deriving (Eq, Show) -- PB = PlayerBlack, PW = PlayerWhite
type PlayerStats = (Player, (Int, Int))
-- First Int is score, 2nd is pass counter... This needs more tinkering
type Position = (Char, Int)
type Board = [[Position]]
type GameState = ([PlayerStats], Board)

-- Define the size of the board here. Future would like to make it user choice
-- where n x n square and n >= 9
boardSize :: Int
boardSize = 9

-- specifically in refernce to the max size of boards list
boardSpaces :: Int
boardSpaces = (boardSize*boardSize)-1

rowSpaces :: Int
rowSpaces = boardSize - 1

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

-- Identify the current player
currentPlayer :: Player -> String
currentPlayer p | p == PB = "BLACK"
                | otherwise = "WHITE"

-- Return the player stone from Stones w/ respect to the current
-- playerID passed in
pStone :: Player -> Char
pStone PB = stone Black
pStone PW = stone White


playerStats :: [PlayerStats]
playerStats = [(PB,(0,0)), (PW,(0,0))]

-- Allows the code to swap between players. White initializes this
-- because the first call to `turnToggle` will have black go first.
playerID :: Player
playerID = PW

scoreState :: GameState -> [PlayerStats]
scoreState = fst

boardState :: GameState -> Board
boardState = snd




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
    else replicate (n-1) [(stone Blank, i) | i <- [0..n-1]]

-- Where it all starts. Recursively runs the game using a do statement.
haskellGo :: GameState -> Player -> IO ()
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
    if not $ checkMove (boardState currentGame) move then do
      _ <- clearScreen
      -- //HACK - Remove these in final version
      -- uncurry (printf "\n\n\n(%d,%d)\n\n\n") move
      printf "\n**ERROR** - Invalid move detected. Please try again.\n"
      let pID'' = turnToggle pID'
      haskellGo currentGame pID''
    else do
      -- //HACK - Remove these in final version
      -- uncurry (printf "\n\n\n(%d,%d)\n\n\n") move
      let captured = capturedStones pID' (scoreState currentGame)
      let newStats = updateStats pID' (scoreState currentGame)
      let newBoard = makeBoard pID' (boardState currentGame) (0, posCalc move)
      let newState = (newStats, newBoard)
      haskellGo newState pID'

-- Quickly get the position in the Board[Position] list being changed
posCalc :: (Int, Int) -> Int
posCalc (x,y) = (x-1)+(y-1)*boardSize

-- make a move and new board based off of playeres coordinates
makeBoard :: Player -> Board -> (Int, Int) -> Board
makeBoard _ [] (_, _)        = []
makeBoard pID (b:bs) (i, pos)
  | i == pos && snd b == pos = (pStone pID, pos):makeBoard pID bs (i+1, pos)
  | i > boardSpaces = []
  | otherwise = b:makeBoard pID bs (i+1, pos)

-- //FIXME this can be removed most likely but hold onto it in the event it
-- inspires something in checkMove later
-- stages the newboard with some small operations.
-- makeMove :: Player -> Board -> (Int,Int) -> Board
-- makeMove pID board pos = makeBoard pID board (0, posCalc pos

{-- //TODO Just do this to check to see if the space is occupied,
  out of bounds, invalid input, etc. User some return tuples to give errors
  lazy but fine--}
checkMove :: Board -> (Int, Int) -> Bool
checkMove board move | move == (-99,-99)  = False -- //TODO increment pass here/maybe do a bit more io
                     | otherwise          = True
capturedStones :: Player -> Board -> Int -> [Int]
capturedStones p b i = undefined


-- playerStats = [(PB,(0,0)), (PW,(0,0))]
updateStats :: Player -> [PlayerStats] -> (Int, Int) -> [PlayerStats]
updateStats _ [] _ = []
updateStats pID (p:ps) mv
  | mv == (-99,-99) && pID == PB = updatePlayerPass p:ps
  | mv == (-99,-99) && pID == PW = p:updateStats pID ps mv
  | otherwise = p:ps

updatePlayerPass :: PlayerStats -> PlayerStats
updatePlayerPass ps = (fst ps, (fst (snd ps), snd (snd ps)+1))


getCoordinates :: IO (Int, Int)
getCoordinates =
  do
    hFlush stdout
    coords <- getLine
    -- //TODO - implement pass once stats is working. Should be easy... I hope
    if contains "pass" $ map toLower coords then return (-99,-99)
    else if contains "quit" $ map toLower coords then exitSuccess
    else do
      -- Higher order! Found a place!
      let coords' = filter (not . null) $ map (filter isDigit) (words coords)
      if length coords' /= 2 then return (-1, -1)
      else do
        let (x, y) = (\b -> (read (head b) ::Int, read (last b) ::Int)) coords'
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
    displayScore (scoreState gameState)
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
        let x = show row ++ " | " ++ rowStates (rowIndexes row) board
        printf "\n%s" x
        displayEachRow (row+1) board
    else printf "\n"

-- get the state of each position in each row
rowStates :: (Int,Int) -> Board -> String
rowStates _ []          = []
rowStates (s,e) (b:bs)
  | snd b < s = rowStates (s,e) bs
  | s <= e = " " ++ fst b : rowStates (s+1,e) bs
  | otherwise           =  [] -- rowStates (s+1,e) bs


-- This took a little to figure out. Each starting and ending index 
-- based on the row being passed in because the Board has n-Positions where
-- (n*n)-1 positions available. (start, end) for the current row
rowIndexes :: Int -> (Int, Int)
rowIndexes row = (row*10-(10+(row-1)), row*10-(row+1))


displayScore :: [PlayerStats] -> IO ()
displayScore stats =
  do
    printf "------------------------\n"
    printf "Score -- b: 0 -- w: 0\n\n"


{-- /*TODO
   function to identify if a user has changed the state in a position on the
    board.
  [ ] Modify board to be a list of lists for positons, each index is a row in
      the range of 0->8 and each row is 0->8.
    [ ] Update display then to work with this.
      [ ] Including properly numbering the rows.
    [ ] Update ??? 
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
