module ConnectFour where

import Data.Word
import Data.Bits
import Data.Maybe
import Data.List
import Data.Ord
import qualified Data.Vector.Unboxed as V

import Negamax

data Player = Human | Computer deriving (Enum, Eq)
-- (rows, columns)
type BoardSize = (Int, Int)
-- board size, line masks, human discs, computer discs
-- !! N.B. this representation fits a board with max. 64 slots !!
data Board = Board BoardSize (V.Vector Word64) Word64 Word64
type Move = Int
data Position = Position Move Player Board

instance GamePosition Position where
    evaluate pos = maybe 0 (\a -> if a == Human then -1 else 1) $ winner pos
    children pos = case winner pos of
        Just _ -> []
        _      -> makeMove pos <$> orderedMoves pos

boardIndex :: BoardSize -> (Int, Int) -> Int
boardIndex (_, nCols) (row, col) = row * nCols + col

discAt :: Board -> (Int, Int) -> Maybe Player
discAt (Board bSize _ human computer) coord
    | testBit human myBit = Just Human
    | testBit computer myBit = Just Computer
    | otherwise = Nothing
    where myBit = boardIndex bSize coord

possibleMoves :: Position -> [Move]
possibleMoves (Position _ _ brd@(Board (nRows, nCols) _ _ _)) =
    filter (\col -> isNothing $ discAt brd (nRows - 1, col)) [0..nCols-1]

-- Apply the move on the position and return the resulting position
makeMove :: Position -> Move -> Position
makeMove (Position _ turn (Board bSize@(nRows, _) masks human computer)) col =
    Position col (nextTurn turn) newBoard
    where newBoard = if turn == Computer
                     then makeNewBoard human $ setBit computer myBit
                     else makeNewBoard (setBit human myBit) computer
          makeNewBoard = Board bSize masks
          myBit = boardIndex bSize (freeRow, col)
          freeRow = fromMaybe 0 $ find (\row -> not
              $ testBit (human .|. computer) (boardIndex bSize (row, col)))
              [0..nRows-1]

emptyBoard :: BoardSize -> Int -> Board
emptyBoard size lineLen = Board size (lineMasks size lineLen) 0 0

nextTurn :: Player -> Player
nextTurn Human    = Computer
nextTurn Computer = Human

-- Create bitmask from an array of coordinates
coordsToMask :: BoardSize -> [(Int, Int)] -> Word64
coordsToMask bSize coords = foldl' setBit 0 $ boardIndex bSize <$> coords

-- Compute all the bitmasks which represent winning lines on the board.
lineMasks :: BoardSize -> Int -> V.Vector Word64
lineMasks bSize@(nRows, nCols) lineLen = V.fromList
    $ coordsToMask bSize <$> allLines
    where allLines = concat [vert, horz, diag1, diag2]
          vert  = [[(r + dl, c) | dl <- line] | r <- rBot, c <- cAll]
          horz  = [[(r, c + dl) | dl <- line] | r <- rAll, c <- cLeft]
          diag1 = [[(r + dl, c + dl) | dl <- line] | r <- rBot, c <- cLeft]
          diag2 = [[(r - dl, c + dl) | dl <- line] | r <- rTop, c <- cLeft]
          cAll  = [0..nCols-1]
          cLeft = [0..nCols-lineLen]
          rAll  = [0..nRows-1]
          rBot  = [0..nRows-lineLen]
          rTop  = [lineLen-1..nRows-1]
          line  = [0..lineLen-1]

-- Test if the word a has all the bits specified by word b set
testMask :: Word64 -> Word64 -> Bool
testMask a b = a .&. b `xor` b == 0

winner :: Position -> Maybe Player
winner (Position _ _ (Board _ masks human computer))
    | hasWinningLine human = Just Human
    | hasWinningLine computer = Just Computer
    | otherwise = Nothing
    where hasWinningLine board = V.any (testMask board) masks

isFull :: Position -> Bool
isFull pos = null $ possibleMoves pos

-- Order moves by how close they are to the center of the board. This usually
-- seems to result in a speedup in the pruning algorithm
orderedMoves :: Position -> [Move]
orderedMoves pos@(Position _ _ (Board (_, nCols) _ _ _)) = sortBy
    (comparing $ \col -> abs (col - nCols `div` 2)) $ possibleMoves pos
