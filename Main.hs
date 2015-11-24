import Data.List
import Data.Maybe
import System.IO
import System.Console.ANSI
import System.Random
import ConnectFour

data Game = Game { redPlayer  :: Player
                 , bluePlayer :: Player
                 , position   :: Position
                 , cursorCol  :: Int }

instance Show Game where
    show game = let (Position turn board) = position game in
        replicate (2 + 2 * cursorCol game) ' ' ++ show turn ++ "\n"
        ++ show board

pad :: Int -> String -> String
pad p str = replicate p '\n' ++
    (unlines . map (replicate (2*p) ' ' ++) . lines $ str)

clamp :: Ord a => a -> a -> a -> a
clamp mn mx = max mn . min mx

movePointer :: Int -> Game -> Game
movePointer dx game = let col = cursorCol game + dx
                          (_, nCols) = boardSize game
                      in game { cursorCol = clamp 0 (nCols-1) col }

boardSize :: Game -> BoardSize
boardSize game = let (Position _ (Board bSize _ _)) = position game in bSize

dropDisc :: Game -> Game
dropDisc game = game { position = newPos }
    where pos = position game
          curCol = cursorCol game
          move = find (\(_, col) -> col == curCol) $ possibleMoves pos
          newPos = fromMaybe pos (makeMove pos <$> move)

gameLoop :: Game -> IO ()
gameLoop game = do
    clearScreen
    setCursorPosition 0 0
    putStr . pad 1 . show $ game
    hFlush stdout
    case winner $ position game of
        Just a -> endGame a
        _      -> getPlayerMove game

getPlayerMove :: Game -> IO ()
getPlayerMove game = do
    chr <- getChar
    case chr of
        'h' -> gameLoop $ movePointer (-1) game
        'l' -> gameLoop $ movePointer 1    game
        ' ' -> gameLoop $ dropDisc game
        'q' -> return ()
        _   -> gameLoop game

endGame :: Disc -> IO ()
endGame player = putStrLn . pad 1
    $ playerName player ++ " player is the winner!"
    where playerName Red'  = "Red"
          playerName Blue' = "Blue"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hideCursor

    startingPlayer <- toEnum <$> randomRIO (0, 1)
    gameLoop Game { redPlayer = Human
                  , bluePlayer = Human
                  , position = Position startingPlayer (emptyBoard (6, 7) 4)
                  , cursorCol = 0 }

    showCursor

