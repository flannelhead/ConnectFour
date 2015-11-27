module ConnectFour where

import qualified Data.Vector         as V
import           Data.Maybe
import           Data.List
import           Data.Ord
import           System.Console.ANSI

data Player = Human | Computer deriving (Enum, Eq)
-- (rows, columns)
type BoardSize = (Int, Int)
-- board size, line length, board vector
data Board = Board BoardSize Int (V.Vector (Maybe Player))
data Position = Position Player Board
-- (row, column)
type Move = (Int, Int)
data GameTree = Node Move Position [GameTree]

instance Show Player where
    show player = setSGRCode [SetColor Foreground Vivid $ color player] ++
        "●" ++ setSGRCode []
        where color Human    = Red
              color Computer = Blue

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

discAt :: Board -> (Int, Int) -> Maybe Player
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

nextTurn :: Player -> Player
nextTurn Human    = Computer
nextTurn Computer = Human

winner :: Position -> Maybe Player
winner (Position _ brd@(Board (nRows, nCols) lineLen _)) = listToMaybe
    $ mapMaybe foldLine allLines
    where foldLine :: [(Int, Int)] -> Maybe Player
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

isFull :: Position -> Bool
isFull (Position _ (Board _ _ vec)) = Nothing `notElem` vec

makeGameTree :: BoardSize -> Int -> Player -> GameTree
makeGameTree size lineLen player = Node (0, 0) firstPos $ nodes firstPos
    where firstPos = Position player $ emptyBoard size lineLen
          nodes :: Position -> [GameTree]
          nodes pos = if isJust $ winner pos then []
              else map (gameTreeFromMove pos) $ possibleMoves pos
          gameTreeFromMove :: Position -> Move -> GameTree
          gameTreeFromMove pos move = Node move newPos $ nodes newPos
              where newPos = makeMove pos move

evaluatePosition :: Position -> Player -> Int
evaluatePosition pos player = maybe 0 (\a -> if a == player then 1 else -1)
    $ winner pos

minimax :: Int -> Player -> GameTree -> Int
minimax 0     player (Node _ pos _)   = evaluatePosition pos player
minimax _     player (Node _ pos [])  = evaluatePosition pos player
minimax depth player (Node _ (Position player2 _) nodes) = minmax
    $ map (minimax (depth - 1) player) nodes
    where minmax = if player2 == player then maximum else minimum

bestMove :: Int -> Player -> GameTree -> GameTree
bestMove depth player (Node _ _ nodes) = maximumBy
    (comparing $ minimax depth player) nodes
