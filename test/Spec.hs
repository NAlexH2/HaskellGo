import GoTests  (
                  testCurrentPlayerStats,
                  testBoardState, 
                  testStatsState,
                  testEmptyBoard,
                  testCurrentRow,
                  testPreviousRow,
                  testNextRow,
                  testGetPosition,
                  testRowLimit,
                  testLegalMove,
                  testUpdatePlayerPass,
                  testGetPassCount,
                  testRowStates,
                  testIdentifyUnits
                )
import Test.HUnit

main :: IO ()
main = do
  _ <- runTestTT $ TestList [ testIdentifyUnits ]
  -- _ <- runTestTT $ TestList 
  --       [ 
  --         testCurrentPlayerStats, testBoardState, testStatsState,
  --         testEmptyBoard, testCurrentRow, testPreviousRow, testNextRow,
  --         testGetPosition, testRowLimit, testLegalMove, testUpdatePlayerPass,
  --         testGetPassCount, testRowStates
  --       ]
  return ()