module ConnectFour where

import qualified Data.Vector         as V
import           Data.Maybe
import           Data.List
import           System.Console.ANSI

data Disc = Red' | Blue' deriving Enum
type Turn = Disc
data Player = Human | Computer
-- (rows, columns)
type BoardSize = (Int, Int)
-- board size, line length, board vector
data Board = Board BoardSize Int (V.Vector (Maybe Disc))
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

