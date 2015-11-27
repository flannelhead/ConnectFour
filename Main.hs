import Control.Monad
import Data.List
import Data.Maybe
import System.IO
import System.Console.ANSI
import System.Random
import ConnectFour

data Game = Game { redPlayer  :: Player
                 , bluePlayer :: Player
                 , message    :: String
                 , gameTree   :: GameTree
                 , cursorCol  :: Int }

getPosition :: Game -> Position
getPosition game = let Node position _ = gameTree game in position

instance Show Game where
    show game = let Position turn board = getPosition game in
        replicate (2 + 2 * cursorCol game) ' ' ++ show turn ++ "\n"
        ++ show board ++ "\n" ++ message game

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
boardSize game = let Position _ (Board bSize _ _) = getPosition game in bSize

dropDisc :: Game -> Game
dropDisc game = game { gameTree = newTree }
    where tree@(Node _ nodes) = gameTree game
          newTree = fromMaybe tree $ snd
            <$> find (\node -> (snd . fst) node == cursorCol game) nodes

gameLoop :: Game -> IO ()
gameLoop game = let pos = getPosition game in case winner pos of
    Just a -> endGameWin game a
    _      -> if isFull pos then endGameTie game else makeNextMove game

drawGame :: Game -> IO ()
drawGame game = do
    clearScreen
    setCursorPosition 0 0
    putStr . pad 1 . show $ game
    hFlush stdout

currentPlayer :: Game -> Player
currentPlayer game = let
    Position turn _ = getPosition game
    player = case turn of
        Red'  -> redPlayer game
        Blue' -> bluePlayer game
    in player

makeNextMove :: Game -> IO ()
makeNextMove game = case currentPlayer game of
    Human    -> makeHumanMove game
    Computer -> makeComputerMove game

makeHumanMove :: Game -> IO ()
makeHumanMove game = do
    drawGame game
        { message = "h/j = move left/right, space = drop disc, q = quit" }
    chr <- getChar
    case chr of
        'h' -> makeHumanMove $ movePointer (-1) game
        'l' -> makeHumanMove $ movePointer 1    game
        ' ' -> gameLoop $ dropDisc game
        'q' -> return ()
        _   -> gameLoop game

makeComputerMove :: Game -> IO ()
makeComputerMove game = do
    drawGame game { message = "Press space to accept computer move"
                  , cursorCol = col }
    waitForSpace
    gameLoop game { gameTree = snd $ head nodes }
    where Node _ nodes = gameTree game
          col = snd . fst $ head nodes
          waitForSpace :: IO ()
          waitForSpace = do
              chr <- getChar
              unless (chr == ' ') waitForSpace

endGameTie :: Game -> IO ()
endGameTie game = drawGame game { message = "It's a tie! " }

endGameWin :: Game -> Disc -> IO ()
endGameWin game player = drawGame
    game { message = playerName player ++ " player wins!" }
    where playerName Red'  = "Red"
          playerName Blue' = "Blue"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hideCursor

    clearScreen
    setCursorPosition 0 0
    putStrLn . pad 1 $ "Let's play Connect Four!\n\n\
                       \Red player = you\nBlue player = computer\n\n\
                       \Press any key to start"
    hFlush stdout
    _ <- getChar

    startingPlayer <- toEnum <$> randomRIO (0, 1)
    gameLoop Game { redPlayer = Human
                  , bluePlayer = Computer
                  , message = ""
                  , gameTree = makeGameTree (6, 7) 4 startingPlayer
                  , cursorCol = 0 }

    showCursor
