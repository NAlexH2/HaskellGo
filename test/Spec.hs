import GoTests
import Test.HUnit

main :: IO ()
main = do
  _ <- runTestTT $ TestList [ testIDUnitLiberties ]
  -- _ <- runTestTT $ TestList 
        -- [ 
          -- testCurrentPlayerStats, testBoardState, testStatsState,
          -- testEmptyBoard, testCurrentRow, testPreviousRow, testNextRow,
          -- testGetNext, testGetPosition, testRowLimit, testPosCalc,
          -- testLegalMove, testUpdatePlayerPass, testGetPassCount,  
          -- testRowStates, testIdentifyUnits, testCappedSingles
        -- ]
  return ()