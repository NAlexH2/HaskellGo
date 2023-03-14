import GoTests
import Test.HUnit ( Test(TestList), runTestTT )

main :: IO ()
main = do
  -- _ <- runTestTT $ TestList [  ]
  _ <- runTestTT $ TestList 
        [ 
          testCurrentPlayerStats, testBoardState, testStatsState,
          testEmptyBoard, testCurrentRow, testPreviousRow, testNextRow,
          testGetPosition, testRowLimit, testPosCalc, testLegalMove, 
          testUpdatePlayerPass, testGetPassCount, testRowStates, 
          testIdentifyUnits, testCappedSingles, testCapturedStones
        ]
  return ()