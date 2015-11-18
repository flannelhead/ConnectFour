import qualified Data.Vector         as V
import           Data.Maybe
import           Data.List
import           System.IO
import           System.Console.ANSI
import           System.Random

data Disc = Red' | Blue' deriving Enum
data Player = Human | Computer
type BoardSize = (Int, Int)
data Board = Board BoardSize (V.Vector (Maybe Disc))
data Game = Game { redPlayer  :: Player
                 , bluePlayer :: Player
                 , turn       :: Disc
                 , lineLength :: Int
                 , board      :: Board }

instance Show Disc where
    show disc = setSGRCode [SetColor Foreground Vivid $ color disc] ++
        "●" ++ setSGRCode []
        where color Red' = Red
              color Blue' = Blue

instance Show Board where
    show brd@(Board (nRows, nCols) _) = unlines . reverse
        $ ('┗' : replicate (2*nCols + 1) '━' ++ "┛")
          : map (\line -> "┃ " ++ line ++ "┃")
            [ concat [ showDisc $ discAt brd (row, col) | col <- [0..nCols-1] ]
              | row <- [0..nRows-1] ]
        where showDisc (Just disc) = show disc ++ " "
              showDisc _           = "  "

boardIndex :: BoardSize -> (Int, Int) -> Int
boardIndex (_, nCols) (row, col) = row * nCols + col

discAt :: Board -> (Int, Int) -> Maybe Disc
discAt (Board bSize vec) (row, col) = vec V.! boardIndex bSize (row, col)

freeRow :: Board -> Int -> Maybe Int
freeRow brd@(Board (nRows, _) _) col =
    find (\row -> isNothing $ discAt brd (row, col)) [0..nRows-1]

putDisc :: Board -> Disc -> Int -> Maybe Board
putDisc brd@(Board bSize vec) disc col =
    (\row -> Board bSize
        (vec V.// [(boardIndex bSize (row, col), Just disc)]))
    <$> freeRow brd col

emptyBoard :: BoardSize -> Board
emptyBoard size@(rows, cols) =
    Board size $ V.replicate (rows * cols) Nothing

nextTurn :: Disc -> Disc
nextTurn Red' = Blue'
nextTurn _    = Red'

playTurn :: Game -> IO Game
playTurn game = do
    col <- randomRIO (0, nCols-1)
    _ <- getChar
    return game { turn = nextTurn curDisc
                , board = fromMaybe curBoard (putDisc curBoard curDisc col) }
    where (_, nCols) = boardSize game
          curBoard = board game
          curDisc = turn game

boardSize :: Game -> BoardSize
boardSize game = let Board bSize _ = board game in bSize

gameLoop :: Game -> IO ()
gameLoop game = do
    clearScreen
    setCursorPosition 0 0
    print $ board game
    hFlush stdout
    playTurn game >>= gameLoop

main :: IO ()
main = do
    -- Console setup
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    startingPlayer <- toEnum <$> randomRIO (0, 1)
    gameLoop Game { redPlayer = Human
                  , bluePlayer = Computer
                  , turn = startingPlayer
                  , lineLength = 4
                  , board = emptyBoard (6, 7) }
