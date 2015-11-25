import Data.List
import Data.Maybe
import System.IO
import System.Console.ANSI
import System.Random
import ConnectFour

data Game = Game { redPlayer  :: Player
                 , bluePlayer :: Player
                 , gameTree   :: GameTree
                 , cursorCol  :: Int }

getPosition :: Game -> Position
getPosition game = let (Node position _) = gameTree game in position

instance Show Game where
    show game = let (Position turn board) = getPosition game in
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
boardSize game = let (Position _ (Board bSize _ _)) = getPosition game in bSize

dropDisc :: Game -> Game
dropDisc game = game { gameTree = newTree }
    where tree@(Node _ nodes) = gameTree game
          newTree = fromMaybe tree $ snd
            <$> find (\node -> (snd . fst) node == cursorCol game) nodes

gameLoop :: Game -> IO ()
gameLoop game = do
    clearScreen
    setCursorPosition 0 0
    putStr . pad 1 . show $ game
    hFlush stdout
    case winner $ getPosition game of
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
    $ playerName player ++ " player wins!"
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
                  , gameTree = makeGameTree (6, 7) 4 startingPlayer
                  , cursorCol = 0 }

    showCursor

