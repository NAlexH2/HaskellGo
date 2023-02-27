-- Alex Harris
-- Final Project: HaskellGo
-- Date Started: 2/25/2023
module Main (main) where

import qualified HaskellGo as HG
import Test.HUnit()
import qualified HaskellGo as HG

main :: IO ()
main = 
  do
    let newGame = (HG.playerStats, HG.emptyBoard)
    HG.haskellGo(HG.emptyBoard HG.boardSize) HG.playerID