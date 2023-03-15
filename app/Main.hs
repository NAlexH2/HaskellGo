-- Alex Harris
-- Final Project: HaskellGo
-- Date Started: 2/25/2023
module Main (main) where

import qualified HaskellGo as HG
import qualified GoWork as GW
import qualified GoConsts as GC
import qualified GoTypesData as GTD

main :: IO ()
main = 
  -- newGame is a GameState starting with empty stats for each player and
  -- an empty board with the designated board size in GoConsts.hs at the top
  do 
    _ <- GW.clearScreen
    let newGame = GW.newState GTD.emptyStats (GW.emptyBoard GC.boardSize)
    HG.haskellGo newGame GC.notFirstPlayer -- Start the game!