module Negamax where

import Control.Monad

class GamePosition a where
    -- Given a position, list possible next positions sorted by the likelihood
    -- they will be good moves
    children :: a -> [a]
    -- Evaluate the score of the position from the point of view of the
    -- maximizing player
    evaluate :: a -> Float

class Semigroup a where
    (<>) :: a -> a -> a

-- This is a type for associating a value with a position in the negamax
-- algorithm
data GamePosition a => AlphaBeta a b = AlphaBeta { getPos :: a, getVal :: b }
-- Let's define a semigroup instance for use in the alpha-beta pruning algo.
-- There we maximize over game positions. A semigroup is a convenient way to
-- express this. There's no natural way to introduce a mempty here, hence
-- this is not a Monoid instance.
instance (GamePosition a, Ord b) => Semigroup (AlphaBeta a b) where
    a <> b = if getVal a >= getVal b then a else b

-- An implementation of the negamax algorithm with alpha-beta pruning
-- https://en.wikipedia.org/wiki/Negamax#Negamax_with_alpha_beta_pruning
negamax :: GamePosition a => Int -> Float -> Float -> Int -> a -> Float
negamax 0     _ _ color pos = fromIntegral color * evaluate pos
negamax depth a b color pos = getVal $ alphaBeta depth a b color pos

-- Let's have some fun with the Either monad in the alpha-beta pruning algorithm
-- Left means that the pruning is finished
-- Right means that the search is going on
-- Now the alpha-beta pruning can be nicely expressed as a fold in terms
-- of the Either monad. The fold _could_ be implemented equally well in
-- without the monad, but the abstraction makes the control flow somewhat
-- clearer by stating explicitly if the pruning is finished in the middle
-- of the fold.
alphaBeta :: GamePosition a => Int -> Float -> Float -> Int -> a
             -> AlphaBeta a Float
alphaBeta depth a b c pos = fromEither $ case children pos of
                                (p:ps) -> foldM f (doNegamax (-1/0) p) ps
                                _      -> Left
                                          $ AlphaBeta pos (negamax 0 a b c pos)
    where doNegamax a2 p = AlphaBeta p $ -negamax (depth-1) (-b) (-a2) (-c) p

          f acc p | getVal acc >= b = Left acc
                  | otherwise = Right $ acc <> newAb
                  where newAb = doNegamax (max a $ getVal acc) p

          fromEither (Left  x) = x
          fromEither (Right x) = x

-- The initial negamax step which returns the chosen position
bestNextPosition :: GamePosition a => Int -> a -> a
bestNextPosition depth pos = getPos $ alphaBeta depth (-1/0) (1/0) 1 pos
