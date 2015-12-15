{-# LANGUAGE BangPatterns, InstanceSigs #-}

import Control.Monad
import System.IO
import System.Console.ANSI
import System.Random

import ConnectFour
import Negamax

data Game = Game { message    :: String
                 , depth      :: Int
                 , position   :: !Position
                 , cursorCol  :: !Int }

-- I've implemented the StateT transformer myself to get some exercise.
-- This was done without looking at the mtl implementation at all (except for
-- the documentation for hints of the types)
data StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Monad m => Monad (StateT s m) where
    return :: a -> StateT s m a
    return x = StateT $ \s -> return (x, s)

    (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
    sx >>= f = StateT $ \s1 -> runStateT sx s1
                     >>= \(a, s2) -> runStateT (f a) s2

instance Monad m => Applicative (StateT s m) where
    pure = return
    ff <*> fx = do { f <- ff; x <- fx; return $ f x }

instance Monad m => Functor (StateT s m) where
    fmap f fx = do { x <- fx; return $ f x }

put :: Monad m => s -> StateT s m ()
put s = StateT $ const $ return ((), s)

get :: Monad m => StateT s m s
get = StateT $ \s -> return (s, s)

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = get >>= \s -> put (f s)

lift :: Monad m => m a -> StateT s m a
lift mx = StateT $ \s -> mx >>= \x -> return (x, s)

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT sx s = runStateT sx s >>= \(x, _) -> return x


instance Show Game where
    show game = let Position _ turn board = position game in
        replicate (2 + 2 * cursorCol game) ' ' ++ showDisc turn ++ "\n"
        ++ showBoard board ++ "\n" ++ message game where
        showDisc player = setSGRCode [SetColor Foreground Vivid $ color player]
            ++ "●" ++ setSGRCode []
            where color Human    = Red
                  color Computer = Blue
        showBoard brd@(Board (nRows, nCols) _ _ _) = unlines . reverse
            $ ('┗' : replicate (2*nCols + 1) '━' ++ "┛")
              : map (\line -> "┃ " ++ line ++ "┃")
                [concat [showDisc2 $ discAt brd (row, col)
                | col <- [0..nCols-1]] | row <- [0..nRows-1]]
            where showDisc2 (Just disc) = showDisc disc ++ " "
                  showDisc2 _           = "  "

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

gameLoop :: StateT Game IO ()
gameLoop = do
    game <- get
    let pos = position game
    case winner pos of
        Just a -> endGameWin a
        _      -> if isFull pos then endGameTie
                                else makeNextMove

drawGame :: StateT Game IO ()
drawGame = do
    !game <- get
    lift $ do clearScreen
              setCursorPosition 0 0
              putStr . pad 1 . show $ game
              hFlush stdout

currentPlayer :: Game -> Player
currentPlayer game = let Position _ player _ = position game in player

makeNextMove :: StateT Game IO ()
makeNextMove = do
    game <- get
    case currentPlayer game of
        Human    -> makeHumanMove
        Computer -> makeComputerMove

makeHumanMove :: StateT Game IO ()
makeHumanMove = do
    modify (setMessage "h/l = move left/right, space = drop disc, q = quit")
    drawGame
    chr <- lift getChar
    case chr of
        'h' -> modify (movePointer (-1)) >> makeHumanMove
        'l' -> modify (movePointer 1) >> makeHumanMove
        ' ' -> modify dropDisc >> gameLoop
        'q' -> return ()
        _   -> gameLoop

setMessage :: String -> Game -> Game
setMessage msg game = game { message = msg }

makeComputerMove :: StateT Game IO ()
makeComputerMove = do
    modify (setMessage "The computer is pondering...")
    drawGame
    game <- get
    let newGame = game { position = nextPos game }
    modify (alignCursor newGame)
    modify (setMessage "Press space to accept the computer's move")
    drawGame
    waitForSpace
    put newGame
    gameLoop
    where alignCursor game1 game2 = let Position col _ _ = position game1 in
                                        game2 { cursorCol = col }
          nextPos game2 = bestNextPosition (depth game2) $ position game2
          waitForSpace = lift getChar >>= \c -> unless (c == ' ') waitForSpace

endGameTie :: StateT Game IO ()
endGameTie = modify (setMessage "It's a tie!") >> drawGame

endGameWin :: Player -> StateT Game IO ()
endGameWin player =
    modify (setMessage ("You " ++ outcome ++ "!\n\n")) >> drawGame
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
    evalStateT gameLoop Game { message = ""
                             , depth = 9
                             , position = Position 0 startingPlayer
                                 $ emptyBoard (6, 7) 4
                             , cursorCol = 0 }
