module Negamax where

import Control.Monad

class GamePosition a where
    -- Given a position, list possible next positions sorted by the likelihood
    -- they will be good moves
    children :: a -> [a]
    -- Evaluate the score of the position from the point of view of the
    -- maximizing player
    evaluate :: a -> Float

-- An implementation of the negamax algorithm with alpha-beta pruning
-- https://en.wikipedia.org/wiki/Negamax#Negamax_with_alpha_beta_pruning
negamax :: GamePosition a => Int -> Float -> Float -> Int -> a -> Float
negamax depth a b color pos
    | depth == 0 || null nodes = fromIntegral color * evaluate pos
    | otherwise = alphaBeta depth a b color nodes
    where nodes = children pos

-- Let's have some fun with the Either monad in the alpha-beta pruning algorithm
-- Here Left means that the pruning is finished
-- Right means that the search is going on
type AlphaBeta = Either Float Float

-- Function to extract the value from the AlphaBeta type
fromAB :: AlphaBeta -> Float
fromAB (Right a) = a
fromAB (Left a)  = a

-- Now the alpha-beta pruning can be nicely expressed as a fold in terms
-- of the AlphaBeta monad. The fold _could_ be implemented equally well in
-- without the monad, but the abstraction makes the control flow somewhat more
-- explicit.
alphaBeta :: GamePosition a => Int -> Float -> Float -> Int -> [a] -> Float
alphaBeta depth a b c ps = fromAB $ foldM f (-1/0) ps
    where f :: GamePosition a => Float -> a -> AlphaBeta
          f val pos
              | val >= b  = Left val  -- could return plain Float values here
                                      -- as well and it would work, but wouldn't
                                      -- be as neat
              | otherwise = Right $ max val newVal
              where newVal = -negamax (depth-1) (-b) (-max a val) (-c) pos

-- The initial call to negamax which returns the chosen position along with the
-- score
bestNextPosition :: GamePosition a => Int -> a -> a
bestNextPosition depth pos = snd . ab (-1/0.0) (-1/0.0, pos) $ children pos
    where ab :: GamePosition a => Float -> (Float, a) -> [a] -> (Float, a)
          ab _ x [] = x
          ab a oldPos@(val, _) (p:ps)
              | val >= 1/0.0 || null ps = oldPos
              | otherwise = ab (max a newVal) newPos ps
              where newVal = -negamax (depth-1) (-1/0.0) (-a) (-1) p
                    newPos = if newVal > val then (newVal, p) else oldPos

