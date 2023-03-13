import GoTests
import Test.HUnit ( Test(TestList), runTestTT )

main :: IO ()
main = do
  _ <- runTestTT $ TestList [ testUpdateStats ]
  -- _ <- runTestTT $ TestList 
  --       [ 
  --         testCurrentPlayerStats, testBoardState, testStatsState,
  --         testEmptyBoard, testCurrentRow, testPreviousRow, testNextRow,
  --         testGetNext, testGetPosition, testRowLimit, testPosCalc,
  --         testLegalMove, testUpdatePlayerPass, testGetPassCount,  
  --         testRowStates, testIdentifyUnits, testCappedSingles,
  --         testCapturedStones
  --       ]
  return ()