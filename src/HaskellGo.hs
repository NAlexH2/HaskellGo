-- Alex Harris
-- Final Project: HaskellGo
-- Date Started: 2/25/2023

module HaskellGo where

import GoWork
import GoTypesData             
import GoConsts
import Text.Printf ( printf )


-- //TODO - Improve comment
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


-- Error handling for the game. Allows user to correct mistakes
errorBadMove :: Int -> String
errorBadMove e  | e == 2      = "Received invalid input from the user."
                | e == 3      = "That space is occupied by the other player."
                | e == 4      = "This move is out of bounds for this board."
                | otherwise   = ""


-- Displays the current state of the game. 
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


-- Displays most up to date state of the board as to be human readable.
displayEachRow :: Int -> Board -> IO ()
displayEachRow row board =
    if row < boardSize+1 then
      do
        let x = show row ++ " | " ++ rowStates (rowLimit row) board
        printf "\n%s" x
        displayEachRow (row+1) board
    else printf "\n"


-- Get the state of each position in a row as a string
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


-- //TODO - Comment
displayScore :: [PlayerStats] -> IO ()
displayScore stats =
  do
    printf "------------------------\n"
    printf "Score -- b: 0 -- w: 0\n\n"


{-- /*TODO
  [ ] Check North, South, East and West liberties using the
      rowLimit. Write an singular function for each of these
      - names: check____ || is____ ||
      [x] Need a function isOccupied :: Board -> (x,y) -> Bool
          - uses posCalc for (x,y) probably
  [ ] Use a GameState versus seperate vars to track the ENTIRE game
      [ ] Update all required members before making new state first
  [ ] Check and capture if liberties are gone
  [ ] Player stats -- kinda has to be done in tandem with capture
  [ ] tbd
  [x] Check if move is valid
  [x] Proper state and display
  [x] Player input
  [x] Player toggler
  */
 --}
