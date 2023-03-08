module GoConsts where
  
import GoTypesData

-- Define the size of the board here. Future would like to make it user choice
-- where n x n square and n >= 9
boardSize :: Int
boardSize = 9


-- specifically in reference to the max size of boards list
-- //TODO - Change this to a counter rather than fixed size to the boardSize
boardSpaces :: Int -> Int
boardSpaces bs = (bs*bs)-1

-- //TODO -- Comment add parameter to this too
rowSpaces :: Int -> Int
rowSpaces bs = bs - 1

-- Allows the code to swap between players. White initializes this
-- because the first call to `turnToggle` will have black go first.
notFirstPlayer :: PlayerID
notFirstPlayer = PW

-- Used to quickly identify if "pass" was entered
pass :: (Int, Int)
pass = (-99, -99)


-- Used to quickly identify if "quit" was entered
quit :: (Int, Int)
quit = (-100, -100)

-- Used to quickly check if there was an error on user input for getCoordinates
badInput :: (Int, Int)
badInput = (-1,-1)