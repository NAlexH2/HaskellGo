-- Alex Harris
-- Final Project: HaskellGo
-- Date Started: 2/25/2023
module Main (main) where

import qualified HaskellGo as HG
import Test.HUnit()

main :: IO ()
main = 
  do
    let newGame = (HG.playerStats, HG.emptyBoard)
    HG.haskellGo newGame HG.playerID