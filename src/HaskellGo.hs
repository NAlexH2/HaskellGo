-- Alex Harris
-- Final Project: HaskellGo
-- Date Started: 2/25/2023

module HaskellGo where

import GoWork
import GoCapture
import GoTypesData
import GoConsts
import Text.Printf ( printf )


-- //TODO - Improve comment
-- Where it all starts. Recursively runs the game using a do statement.
haskellGo :: GameState -> PlayerID -> IO ()
haskellGo currentGame pID =
  do
    let board = boardState currentGame
    let stats = statsState currentGame
    -- //HACK - Remove these in final version
    -- print ((length board)-1)
    -- print (board !! 80)
    -- print (show (boardState currentGame))
    printf "Type 'quit' anytime to end the game or 'pass' to skip your turn.\n"
    printf "To make your move, simply type an x and y that is on the grid. "
    printf "Like so:\n'x y: 9 9'\n\n"
    let pID' = turnToggle pID
    displayState bdSz currentGame
    printf "It is player %s's turn...\n" (currentPlayer pID')
    putStr "x y: "
    move <- getCoordinates
    -- //HACK - remove in final version
    -- print (show (statsState currentGame))
    -- uncurry (printf "\n\n\n(%d,%d) %d\n\n\n") move (posCalc move)
    let legality = legalMove bdSz board move
    if legality > 1 then do
      _ <- clearScreen
      -- //HACK - Remove these in final version
      -- uncurry (printf "\n\n\n(%d,%d)\n\n\n") move
      printf "\n**ERROR** - %s\n" (errorBadMove legality)
      let pID'' = turnToggle pID'
      haskellGo currentGame pID''
    else do
      -- //TODO see if player passed 2 times, end game if so.
      let stats' = updatePlayerPass pID' move stats
      -- //TODO check pass count here
      let board' = makeBoard bdSz pID' board (posCalc move bdSz) []
      let captured = capturedStones bdSz (turnToggle pID') board
      let newStats = updateStats pID' stats' captured
      let newBd = makeBoard bdSz pID' board' (posCalc move bdSz) captured
      let currentGame' = updateGame newStats newBd
      haskellGo currentGame' pID'
  where
    bdSz = boardSize


-- Error handling for the game. Allows user to correct mistakes
errorBadMove :: Int -> String
errorBadMove e  | e == 2      = "Received invalid input from the user."
                | e == 3      = "That space is occupied by the other player."
                | e == 4      = "This move is out of bounds for this board."
                | otherwise   = ""


-- Displays the current state of the game. 
displayState :: Int -> GameState -> IO ()
displayState bdSz gameState =
  do
    displayTopRows
    displayEachRow bdSz row (boardState gameState)
    displayScore (statsState gameState)
      where
          row = 1


-- Visualization of the board to the user for x y coordinates to be entered
displayTopRows :: IO ()
displayTopRows = printf "  x  1 2 3 4 5 6 7 8 9\ny   __________________"


-- Displays most up to date state of the board as to be human readable.
displayEachRow :: Int -> Int -> Board -> IO ()
displayEachRow bdSz row board =
    if row < bdSz+1 then
      do
        let x = show row ++ " | " ++ rowStates (rowLimit bdSz row) board
        printf "\n%s" x
        displayEachRow bdSz (row+1) board
    else printf "\n"


-- Print the current pass count and score for each player in the game.
displayScore :: [PlayerStats] -> IO ()
displayScore stats =
  do
    printf "---------------------------------\n"
    printf "      Score -- b: %d -- w: %d\n\n" sB sW
    printf "Pass Counts -- b: %d -- w: %d\n\n" pB pW
  where
    sB = getPlayerScore PB stats
    sW = getPlayerScore PW stats
    pB = getPassCount PB stats
    pW = getPassCount PW stats


{-- /*TODO
  [ ] Check pass counter and stop the game if either player has a count of
      2.
  [ ] Tabulate the score when the game ends either with pass = 2 or quit
  [ ] Write a big test to check all this?

  
  *** COMPLETED TASKS ***
  [x] Display stats (including pass count)
  [x] Must update the board with new move first, check the board for captures,
      then update the stats/board/etc blah blah
  [x] Ignore/Remove captured stones from the board.
  [x] Player stats -- Length of captured stones increments current players score
  [x] Check unit liberties and single stone liberties
    [x] Unit liberties allow a function to run and see if all those liberties
        are captured. A list of stones that are apart of a unit and then
        checking all of those stones liberties.
    [x] True for each positions stone losing all its liberties, false otherwise
    [x] If any false, do nothing. If all true, mark all as captured. 
    [x] Single stone liberties are easy.
    [x] If they aren't apart of a unit and all their liberties are gone,
        then mark as captured!
  [x] When checking liberties, check to see if the surrounding stones are the
      same color or not. If all (except for the stone we came from) are opposite
      color, then it's captured. Otherwise, it's still free.
      - The value for previous node could literally be prev passed in as an arg
      [x] Check North, South, East and West liberties using the
          rowLimit. Write an singular function for each of these
          [x] Need a function isOccupied :: Board -> (x,y) -> Bool
  [x] Identify units! WOO!
  [x] Write up for checkpoint
  [x] Write tests for the checkpoint
  [x] Use a GameState versus separate vars to track the ENTIRE game
      [x] Update all required members before making new state first
  [x] Implement row addition to position? Or create a working calc for any
      value... I like this option. `= floor (pos/boardSize)` ?
      [x] wound up realizing simplified version! Easy done.
  [x] Check if move is valid
  [x] Proper state and display
  [x] Player input
  [x] Player toggler
  */
 --}
