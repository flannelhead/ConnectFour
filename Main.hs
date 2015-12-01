{-# LANGUAGE BangPatterns #-}

import Control.Monad
import System.IO
import System.Console.ANSI
import System.Random

import ConnectFour
import Negamax

data Game = Game { message    :: String
                 , depth      :: Int
                 , position   :: Position
                 , cursorCol  :: !Int }

instance Show Game where
    show game = let Position _ turn board = position game in
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
boardSize game = let Position _ _ (Board bSize _ _ _) = position game in bSize

dropDisc :: Game -> Game
dropDisc game
    | cursorCol game `elem` possibleMoves (position game) =
        game { position = makeMove (position game) (cursorCol game) }
    | otherwise = game

gameLoop :: Game -> IO ()
gameLoop game = let pos = position game in case winner pos of
    Just a -> endGameWin game a
    _      -> if isFull pos then endGameTie game else makeNextMove game

drawGame :: Game -> IO ()
drawGame !game = do
    clearScreen
    setCursorPosition 0 0
    putStr . pad 1 . show $ game
    hFlush stdout

currentPlayer :: Game -> Player
currentPlayer game = let Position _ player _ = position game in player

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
    drawGame game { message = "The computer is pondering..." }
    drawGame game { message = "Press space to accept computer move"
                  , cursorCol = col }
    waitForSpace
    gameLoop game { position = newPos }
    where newPos@(Position col _ _) = bestNextPosition (depth game)
              $ position game
          waitForSpace :: IO ()
          waitForSpace = do
              chr <- getChar
              unless (chr == ' ') waitForSpace

endGameTie :: Game -> IO ()
endGameTie game = drawGame game { message = "It's a tie! " }

endGameWin :: Game -> Player -> IO ()
endGameWin game player = drawGame
    game { message = "You " ++ outcome ++ "!\n\n" }
    where outcome = if player == Human then "win" else "lose"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    clearScreen
    setCursorPosition 0 0
    putStrLn . pad 1 $ "Let's play Connect Four!\n\n\
                       \Red player = you\n\
                       \Blue player = computer\n\n\
                       \Press any key to start"
    hFlush stdout
    _ <- getChar

    startingPlayer <- toEnum <$> randomRIO (0, 1)
    gameLoop Game { message = ""
                  , depth = 9
                  , position = Position 0 startingPlayer $ emptyBoard (6, 7) 4
                  , cursorCol = 0 }
