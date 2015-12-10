module Negamax where

import Data.List

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
negamax 0     _ _ color pos = fromIntegral color * evaluate pos
negamax depth a b color pos = snd $ alphaBeta depth a b color pos

alphaBeta :: GamePosition a => Int -> Float -> Float -> Int -> a -> (a, Float)
alphaBeta depth a b c pos = case children pos of
                                (p:ps) -> foldl' f (doNegamax (-1/0) p) ps
                                _      -> (pos, negamax 0 a b c pos)
    where doNegamax a2 p = (p, -negamax (depth-1) (-b) (-a2) (-c) p)
          f acc p | snd acc >= b = acc
                  | otherwise = if snd acc >= snd newVal then acc else newVal
                  where newVal = doNegamax (max a $ snd acc) p

-- The initial call to negamax which returns the chosen position along with the
-- score
bestNextPosition :: GamePosition a => Int -> a -> a
bestNextPosition depth pos = fst $ alphaBeta depth (-1/0) (1/0) 1 pos
