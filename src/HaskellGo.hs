-- Alex Harris
-- Final Project: HaskellGo
-- Date Started: 2/25/2023

module HaskellGo where

import GoWork
    ( rowLimit,
      rowStates,
      getPlayerScore,
      updateGame,
      updateStats,
      posCalc,
      makeBoard,
      clearScreen,
      legalMove,
      getPassCount,
      updatePlayerPass,
      getCoordinates,
      currentPlayer,
      turnToggle,
      statsState,
      boardState )
import GoCapture ( capturedStones )
import GoTypesData ( PlayerStats, Board, PlayerID(..), GameState )
import GoConsts ( boardSize, quit )
import Text.Printf ( printf )
import System.Exit (exitSuccess)


-- Where it all starts. Recursively runs the game using a do statement while
-- performing the work and logic before and after the player enters their
-- move. This takes a GameState and PlayerID: GameState is a tuple of the board
-- and players score, and PlayerID is either PB or PW.
haskellGo :: GameState -> PlayerID -> IO ()
haskellGo currentGame pID =
  do
    let board = boardState currentGame
    let stats = statsState currentGame
    let pID' = turnToggle pID

    printf "Type 'quit' anytime to end the game or 'pass' to skip your turn.\n"
    printf "To make your move, simply type an x and y that is on the grid. "
    printf "Like so:\n'x y: 1 1'\n\n"
    displayState bdSz stats board
    printf "It is player %s's turn...\n" (currentPlayer pID')
    putStr "x y: "
    move <- getCoordinates bdSz
    let stats' = updatePlayerPass pID' move stats

    if (getPassCount pID' stats' >= 2) || move == quit 
      then endGame bdSz stats' board

    else do
      let legality = legalMove bdSz board move
      if legality > 1 then do
        _ <- clearScreen
        printf "\n**ERROR** - %s\n" (errorBadMove legality)
        let pID'' = turnToggle pID'
        haskellGo currentGame pID''

      else do
        _ <- clearScreen
        let movedBoard = makeBoard bdSz pID' board (posCalc move bdSz) []
        let captured = capturedStones bdSz (turnToggle pID') movedBoard
        let newStats = updateStats pID' stats' captured
        let newBd = makeBoard bdSz pID' movedBoard (posCalc move bdSz) captured
        let updatedGame = updateGame newStats newBd
        haskellGo updatedGame pID'
  where
    bdSz = boardSize


-- Runs the endgame steps to display the final board and examine the score
endGame :: Int -> [PlayerStats] -> Board -> IO()
endGame bdSz stats board =
  do 
    _ <- clearScreen
    printf "\n***The game is OVER!***\n\n"
    displayState bdSz stats board
    winner stats
    exitSuccess


-- Displays the winner based on current score
winner :: [PlayerStats] -> IO()
winner stats 
  | sB > sW   = printf "\n!!!!BLACK WINS!!!!\n\n"
  | sB < sW   = printf "\n!!!!WHITE WINS!!!!\n\n"
  | otherwise = printf "\n***THE GAME IS TIED***\n\n"
  where
    sB = getPlayerScore PB stats
    sW = getPlayerScore PW stats


-- Error handling for the game. Allows user to correct mistakes
errorBadMove :: Int -> String
errorBadMove e  | e == 2      = "Received invalid input from the user."
                | e == 3      = "That space is occupied by the other player."
                | e == 4      = "This move is out of bounds for this board."
                | otherwise   = ""


-- Displays the current state of the game. 
displayState :: Int -> [PlayerStats] -> Board -> IO ()
displayState bdSz stats board =
  do
    displayTopRows bdSz
    displayEachRow bdSz row board
    displayScore stats
      where
          row = 0


-- Visualization of the board to the user for x y coordinates to be entered
displayTopRows :: Int -> IO ()
displayTopRows bdSz = printf "  x %s\ny   %s" nums line
  where
    nums = concat [" " ++ show n | n <- [1..bdSz]]
    line = concat $ replicate (bdSz*2) "_"


-- Displays most up to date state of the board as to be human readable.
displayEachRow :: Int -> Int -> Board -> IO ()
displayEachRow bdSz row board =
    if row /= bdSz then
      do
        let x = show (row+1) ++ " | " ++ rowStates (rowLimit bdSz row) board
        printf "\n%s" x
        displayEachRow bdSz (row+1) board
    else printf "\n"


-- Print the current pass count and score for each player in the game.
displayScore :: [PlayerStats] -> IO ()
displayScore stats =
  do
    printf "-----------------------\n"
    printf "Score | Pass Count\n"
    printf "b:  %d | %d\n" sB pB
    printf "w:  %d | %d\n\n" sW pW
  where
    sB = getPlayerScore PB stats
    sW = getPlayerScore PW stats
    pB = getPassCount PB stats
    pW = getPassCount PW stats
