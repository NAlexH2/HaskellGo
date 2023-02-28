module GoTypesData where


data Stones = Blank | Black | White | TBlack | TWhite deriving (Eq, Show)
data PlayerID = PB | PW deriving (Eq, Show) -- PB = PlayerBlack, PW = PlayerWhite
type PlayerStats = (PlayerID, (Int, Int))
-- First Int is score, 2nd is pass counter... This needs more tinkering
type Position = (Char, Int)
type Board = [Position]
type GameState = ([PlayerStats], Board)
-- playerStats = [(PB,(0,0)), (PW,(0,0))]

-- Just some constructors for each char in the type position
stone :: Stones -> Char
stone Blank = '_'
stone Black = 'b'
stone TBlack = 'B'
stone White = 'w'
stone TWhite = 'W'


-- //TODO - Comment
emptyStats :: [PlayerStats]
emptyStats = [(PB,(0,0)), (PW,(0,0))]