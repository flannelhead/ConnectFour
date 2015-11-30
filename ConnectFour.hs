module ConnectFour where

import Data.Word
import Data.Bits
import Data.Maybe
import Data.List
import Data.Ord
import qualified Data.Vector.Unboxed as V
import System.Console.ANSI

data Player = Human | Computer deriving (Enum, Eq)
-- (rows, columns)
type BoardSize = (Int, Int)
-- board size, line masks, (human discs, computer discs)
data Board = Board BoardSize (V.Vector Word64) (Word64, Word64)
data Position = Position Player Board
type Move = Int

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
discAt (Board bSize _ (human, computer)) coord
    | testBit human myBit = Just Human
    | testBit computer myBit = Just Computer
    | otherwise = Nothing
    where myBit = boardIndex bSize coord

possibleMoves :: Position -> [Move]
possibleMoves (Position _ brd@(Board (nRows, nCols) _ _)) =
    filter (\col -> isNothing $ discAt brd (nRows - 1, col)) [0..nCols-1]

orderedMoves :: Position -> [Move]
orderedMoves pos@(Position _ (Board (_, nCols) _ _)) =
    sortBy (comparing $ \col -> abs (col - nCols `div` 2)) $ possibleMoves pos

makeMove :: Position -> Move -> Position
makeMove (Position turn
    (Board bSize@(nRows, _) masks (human, computer))) col =
    Position (nextTurn turn) $ Board bSize masks newBoard
    where newBoard = if turn == Computer then (human, setBit computer myBit)
                     else (setBit human myBit, computer)
          myBit = boardIndex bSize (freeRow, col)
          freeRow = fromMaybe 0 $ find (\row -> not
              $ testBit human (boardIndex bSize (row, col))
              || testBit computer (boardIndex bSize (row, col))) [0..nRows-1]

emptyBoard :: BoardSize -> Int -> Board
emptyBoard size lineLen = Board size (lineMasks size lineLen) (0, 0)

nextTurn :: Player -> Player
nextTurn Human    = Computer
nextTurn Computer = Human

testMask :: Word64 -> Word64 -> Bool
testMask a b = a .&. b `xor` b == 0

coordsToMask :: BoardSize -> [(Int, Int)] -> Word64
coordsToMask bSize coords = foldl' setBit 0 $ map (boardIndex bSize) coords

lineMasks :: BoardSize -> Int -> V.Vector Word64
lineMasks bSize@(nRows, nCols) lineLen = V.fromList
    $ map (coordsToMask bSize) allLines
    where allLines = concat [vert, horz, diag1, diag2]
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

winner :: Position -> Maybe Player
winner (Position _ (Board _ masks (human, computer)))
    | hasWinningLine human = Just Human
    | hasWinningLine computer = Just Computer
    | otherwise = Nothing
    where hasWinningLine board = V.any (testMask board) masks

isFull :: Position -> Bool
isFull pos = null $ possibleMoves pos

evaluatePosition :: Position -> Int
evaluatePosition pos = maybe 0 (\a -> if a == Human then -1 else 1) $ winner pos

children :: Position -> [Position]
children pos = case winner pos of
    Just _ -> []
    _      -> map (makeMove pos) $ orderedMoves pos

negamax :: Int -> Int -> Int -> Int -> Position -> Int
negamax depth a b color pos
    | depth == 0 || null nodes = color * evaluatePosition pos
    | otherwise = alphaBeta a (-1) nodes
    where nodes = children pos
          alphaBeta :: Int -> Int -> [Position] -> Int
          alphaBeta _  val [] = val
          alphaBeta a2 val (p:ps)
              | val >= b || null ps = val
              | otherwise = alphaBeta (max a2 newVal) (max val newVal) ps
              where newVal = (-1) * negamax (depth-1) (-b) (-a2) (-color) p

bestMove :: Int -> Position -> (Position, Move)
bestMove depth pos = minimumBy (comparing $ negamax depth (-1) 1 (-1) . fst)
    $ map (\move -> (makeMove pos move, move)) $ orderedMoves pos
