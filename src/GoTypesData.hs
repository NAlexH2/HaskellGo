module GoTypesData where


data Stones = Blank | Black | White deriving (Eq, Show)
data PlayerID = PB | PW deriving (Eq, Show) -- PB = PlayerBlack, PW = PlayerWhite

-- PlayerID is either PB or PW, 1st Int is score, 2nd is pass counter
type PlayerStats = (PlayerID, (Int, Int))

-- Char is state of the position, 1st Int is the row, 
-- 2nd Int is position in the list
type Position = (Char, (Int,Int))
type Board = [Position]
type GameState = ([PlayerStats], Board)

-- Just some constructors for each char in the type position
stone :: Stones -> Char
stone Blank = '_'
stone Black = 'b'
stone White = 'w'


-- //TODO - Comment
emptyStats :: [PlayerStats]
emptyStats = [(PB,(0,0)), (PW,(0,0))]