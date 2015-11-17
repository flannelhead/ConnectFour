{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE TypeSynonymInstances #-}

import qualified Data.Vector         as V
import           System.IO
import           System.Console.ANSI
import           System.Random

data Disc = Red' | Blue' deriving Enum
type Turn = Disc
data Player = Human | Computer
type Cell = Maybe Disc
type BoardSize = (Int, Int)
data Board = Board BoardSize (V.Vector Cell)
data Game = Game { redPlayer  :: Player
                 , bluePlayer :: Player
                 , turn       :: Turn
                 , lineLength :: Int
                 , board      :: Board }

instance Show Disc where
    show disc = setSGRCode [SetColor Foreground Vivid $ color disc] ++
        "*" ++ setSGRCode []
            where color Red' = Red
                  color Blue' = Blue

instance Show Cell where
    show (Just disc) = show disc
    show _           = " "

instance Show Board where
    show (Board (nRows, nCols) vec) = ""

emptyBoard :: BoardSize -> Board
emptyBoard size@(rows, cols) =
    Board size $ V.replicate (rows * cols) Nothing

nextTurn :: Turn -> Turn
nextTurn Red' = Blue'
nextTurn _    = Red'

playTurn :: Game -> IO Game
playTurn game = do
    _ <- getChar
    return game { turn = nextTurn $ turn game }

gameLoop :: Game -> IO ()
gameLoop game = do
    clearScreen
    setCursorPosition 0 0
    print $ turn game
    hFlush stdout
    playTurn game >>= gameLoop

main :: IO ()
main = do
    -- Console setup
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    startingPlayer <- toEnum <$> randomRIO (0, 1)
    gameLoop Game { redPlayer = Human
                  , bluePlayer = Computer
                  , turn = startingPlayer
                  , lineLength = 4
                  , board = emptyBoard (6, 7) }
