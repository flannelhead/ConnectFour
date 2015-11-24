import qualified Data.Vector         as V
import           Data.Maybe
import           Data.List
import           System.IO
import           System.Console.ANSI
import           System.Random

data Disc = Red' | Blue' deriving Enum
type Turn = Disc
data Player = Human | Computer
-- (rows, columns)
type BoardSize = (Int, Int)
-- board size, line length, board vector
data Board = Board BoardSize Int (V.Vector (Maybe Disc))
data Game = Game { redPlayer  :: Player
                 , bluePlayer :: Player
                 , position   :: Position
                 , cursorCol  :: Int }
data Position = Position Turn Board
-- row, column
type Move = (Int, Int)

instance Show Disc where
    show disc = setSGRCode [SetColor Foreground Vivid $ color disc] ++
        "●" ++ setSGRCode []
        where color Red' = Red
              color Blue' = Blue

instance Show Board where
    show brd@(Board (nRows, nCols) _ _) = unlines . reverse
        $ ('┗' : replicate (2*nCols + 1) '━' ++ "┛")
          : map (\line -> "┃ " ++ line ++ "┃")
            [ concat [ showDisc $ discAt brd (row, col) | col <- [0..nCols-1] ]
              | row <- [0..nRows-1] ]
        where showDisc (Just disc) = show disc ++ " "
              showDisc _           = "  "

instance Show Game where
    show game = let (Position turn board) = position game in
        replicate (2 + 2 * cursorCol game) ' ' ++ show turn ++ "\n"
        ++ show board

boardIndex :: BoardSize -> (Int, Int) -> Int
boardIndex (_, nCols) (row, col) = row * nCols + col

discAt :: Board -> (Int, Int) -> Maybe Disc
discAt (Board bSize _ vec) (row, col) = vec V.! boardIndex bSize (row, col)

possibleMoves :: Position -> [Move]
possibleMoves (Position _ brd@(Board (_, nCols) _ _)) =
    mapMaybe (\col -> (\row -> (row, col)) <$> freeRow brd col) [0..nCols-1]
    where freeRow :: Board -> Int -> Maybe Int
          freeRow brd2@(Board (nRows, _) _ _) col =
            find (\row -> isNothing $ discAt brd2 (row, col)) [0..nRows-1]

makeMove :: Position -> Move -> Position
makeMove (Position trn (Board bSize lineLen vec)) move = Position (nextTurn trn)
    $ Board bSize lineLen $ vec V.// [(boardIndex bSize move, Just trn)]

emptyBoard :: BoardSize -> Int -> Board
emptyBoard size@(rows, cols) lineLen =
    Board size lineLen $ V.replicate (rows * cols) Nothing

nextTurn :: Turn -> Turn
nextTurn Red' = Blue'
nextTurn _    = Red'

dropDisc :: Game -> Game
dropDisc game = game { position = newPos }
    where pos = position game
          curCol = cursorCol game
          move = find (\(_, col) -> col == curCol) $ possibleMoves pos
          newPos = fromMaybe pos (makeMove pos <$> move)

clamp :: Ord a => a -> a -> a -> a
clamp mn mx = max mn . min mx

movePointer :: Int -> Game -> Game
movePointer dx game = let col = cursorCol game + dx
                          (_, nCols) = boardSize game
                      in game { cursorCol = clamp 0 (nCols-1) col }

boardSize :: Game -> BoardSize
boardSize game = let (Position _ (Board bSize _ _)) = position game in bSize

gameLoop :: Game -> IO ()
gameLoop game = do
    clearScreen
    setCursorPosition 0 0
    print game
    hFlush stdout
    chr <- getChar
    case chr of
        'h'  -> gameLoop $ movePointer (-1) game
        'l'  -> gameLoop $ movePointer    1 game
        '\n' -> gameLoop $ dropDisc game
        'q'  -> return ()
        _    -> gameLoop game

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hideCursor

    startingPlayer <- toEnum <$> randomRIO (0, 1)
    gameLoop Game { redPlayer = Human
                  , bluePlayer = Computer
                  , position = Position startingPlayer (emptyBoard (6, 7) 4)
                  , cursorCol = 0 }

    showCursor
