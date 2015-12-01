module Negamax where

class GamePosition a where
    children :: a -> [a]
    evaluate :: a -> Float

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

