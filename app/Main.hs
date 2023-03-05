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
  do
    let newGame = GW.newState GTD.emptyStats (GW.emptyBoard GC.boardSize)
    HG.haskellGo newGame GC.notFirstPlayer