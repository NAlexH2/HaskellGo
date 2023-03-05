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
    -- print (show (boardState currentGame))
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


-- //TODO - Comment
displayScore :: [PlayerStats] -> IO ()
displayScore stats =
  do
    printf "------------------------\n"
    printf "Score -- b: %d -- w: %d\n\n" p1 p2
  where
    p1 = 0 :: Int
    p2 = 0 :: Int


{-- /*TODO
  [ ] Check unit liberties and single stone liberties
      [ ] Unit liberties allow a funciton to run and see if all those liberties
        are captured. A list of stones that are apart of a unit and then
          checking all of those stones liberties.
      [ ] True for each positions stone losing all its liberties, false otherwise
      [ ] If any false, do nothing. If all true, mark all as captured. 
        [ ] All of the above can be done in a do using <- probably?
      [ ] Single stone liberties are easy.
        [ ] If they aren't apart of a unit and all their liberties are gone,
            then mark as captured!
  [ ] When checking liberties, check to see if the surrounding stones are the
      same color or not. If all (except for the stone we came from) are opposite
      color, then it's captured. Otherwise, it's still free.
      - The value for previous node could literally be prev passed in as an arg
      [x] Check North, South, East and West liberties using the
          rowLimit. Write an singular function for each of these
          [x] Need a function isOccupied :: Board -> (x,y) -> Bool
  [ ] Check and capture if liberties are gone
  [ ] Some how check territories? Maybe?
    [ ] Or just stick with the "if it is shared, both get a point"?
    [ ] This honestly might just come down to capture count...
  [ ] Player stats -- kinda has to be done in tandem with capture

  
  *** COMPLETED TASKS ***
  [x] Write up for checkpoint
  [x] Write tests for the checkpoint
  [x] Use a GameState versus seperate vars to track the ENTIRE game
      [x] Update all required members before making new state first
  [x] Implement row addition to position? Or create a working calc for any
      value... I like this option. `= floor (pos/boardsize)` ?
      [x] wound up realizing simplified version! Easy done.
  [x] Check if move is valid
  [x] Proper state and display
  [x] Player input
  [x] Player toggler
  */
 --}
