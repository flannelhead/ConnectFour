module ConnectFour where

import qualified Data.Vector         as V
import           Data.Maybe
import           Data.List
import           System.Console.ANSI

data Disc = Red' | Blue' deriving (Enum, Eq)
type Turn = Disc
data Player = Human | Computer
-- (rows, columns)
type BoardSize = (Int, Int)
-- board size, line length, board vector
data Board = Board BoardSize Int (V.Vector (Maybe Disc))
data Position = Position Turn Board
-- row, column
type Move = (Int, Int)
data GameTree = Node Position [(Move, GameTree)]

instance Show Disc where
    show disc = setSGRCode [SetColor Foreground Vivid $ color disc] ++
        "●" ++ setSGRCode []
        where color Red' = Red
              color Blue' = Blue

instance Show Board where
    show brd@(Board (nRows, nCols) _ _) = unlines . reverse
        $ ('┗' : replicate (2*nCols + 1) '━' ++ "┛")
          : map (\line -> "┃ " ++ line ++ "┃")
            [concat [showDisc $ discAt brd (row, col) | col <- [0..nCols-1]]
            | row <- [0..nRows-1]]
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

winner :: Position -> Maybe Disc
winner (Position _ brd@(Board (nRows, nCols) lineLen _)) = listToMaybe
    $ mapMaybe foldLine allLines
    where foldLine :: [(Int, Int)] -> Maybe Disc
          foldLine ln = foldl1 acc $ map (discAt brd) ln
              where acc Nothing _ = Nothing
                    acc a       b = if a == b then a else Nothing
          allLines = concat [vert, horz, diag1, diag2]
          vert = [[(r + dl, c) | dl <- line] | r <- rBot, c <- cAll]
          horz = [[(r, c + dl) | dl <- line] | r <- rAll, c <- cLeft]
          diag1 = [[(r + dl, c + dl) | dl <- line] | r <- rBot, c <- cLeft]
          diag2 = [[(r - dl, c + dl) | dl <- line] | r <- rTop, c <- cLeft]
          cAll = [0..nCols-1]
          cLeft = [0..nCols-lineLen]
          rAll = [0..nRows-1]
          rBot = [0..nRows-lineLen]
          rTop = [lineLen-1..nRows-1]
          line = [0..lineLen-1]

makeGameTree :: BoardSize -> Int -> Turn -> GameTree
makeGameTree size lineLen turn = Node firstPos $ nodes firstPos
    where firstPos = Position turn $ emptyBoard size lineLen
          nodes :: Position -> [(Move, GameTree)]
          nodes pos = map (\move -> (move, gameTreeFromMove pos move))
            $ possibleMoves pos
          gameTreeFromMove :: Position -> Move -> GameTree
          gameTreeFromMove pos move = Node newPos $ nodes newPos
            where newPos = makeMove pos move

