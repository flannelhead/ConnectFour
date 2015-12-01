module Negamax where

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
    | otherwise = alphaBeta a (-1/0.0) nodes
    where nodes = children pos
          alphaBeta :: GamePosition a => Float -> Float -> [a] -> Float
          alphaBeta _  val [] = val
          alphaBeta a2 val (p:ps)
              | val >= b || null ps = val
              | otherwise = alphaBeta (max a2 newVal) (max val newVal) ps
              where newVal = -negamax (depth-1) (-b) (-a2) (-color) p

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

