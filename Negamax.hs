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

data GamePosition a => AlphaBeta a = AlphaBeta { getPos :: a, getVal :: Float }
-- Let's define a semigroup instance for use in the alpha-beta pruning algo.
-- There we maximize over game positions. A semigroup is a convenient way to
-- express this. There's no natural way to introduce a mempty here, hence
-- this is not a Monoid instance.
instance GamePosition a => Semigroup (AlphaBeta a) where
    a <> b = if getVal a >= getVal b then a else b

-- An implementation of the negamax algorithm with alpha-beta pruning
-- https://en.wikipedia.org/wiki/Negamax#Negamax_with_alpha_beta_pruning
negamax :: GamePosition a => Int -> Float -> Float -> Int -> a -> Float
negamax depth a b color pos
    | depth == 0 || null nodes = fromIntegral color * evaluate pos
    | otherwise = getVal $ alphaBeta depth a b color nodes
    where nodes = children pos

-- Let's have some fun with the Either monad in the alpha-beta pruning algorithm
-- Left means that the pruning is finished
-- Right means that the search is going on
-- Now the alpha-beta pruning can be nicely expressed as a fold in terms
-- of the AlphaBeta monad. The fold _could_ be implemented equally well in
-- without the monad, but the abstraction makes the control flow somewhat
-- clearer by stating explicitly if the pruning is finished in the middle
-- of the fold.
-- N.B. This function must be applied to a nonempty list [a]!
alphaBeta :: GamePosition a => Int -> Float -> Float -> Int -> [a]
             -> AlphaBeta a
alphaBeta depth a b c (p:ps) = fromEither $ foldM f (doNegamax (-1/0) p) ps
    where doNegamax :: GamePosition a => Float -> a -> AlphaBeta a
          doNegamax a2 pos = AlphaBeta pos
                             $ -negamax (depth-1) (-b) (-a2) (-c) pos

          f :: GamePosition a => AlphaBeta a -> a
               -> Either (AlphaBeta a) (AlphaBeta a)
          f acc pos
              | getVal acc >= b = Left acc
              | otherwise = Right $ acc <> newAb
              where newAb = doNegamax (max a (getVal acc)) pos

          fromEither (Left  x) = x
          fromEither (Right x) = x

-- The initial call to negamax which returns the chosen position along with the
-- score
bestNextPosition :: GamePosition a => Int -> a -> a
bestNextPosition depth pos = getPos $ alphaBeta depth (-1/0) (1/0) 1
                                    $ children pos

