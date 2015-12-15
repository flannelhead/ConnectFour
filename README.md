# ConnectFour
A small implementation of Connect Four game with beautiful ANSI terminal
graphics and a computer opponent. This project was started on a university
programming course and has proven to be a great way to learn more about
Haskell's powerful abstractions.

NOTE: If the computer turn takes too long to run / swallows too much memory,
adjust the search depth determined at the bottom of `Main.hs`.

## Structure
`Main.hs` contains most of the gameplay and console drawing logic. The game
logic and IO were originally written in terms of plain `IO ()` but have since
been lifted to `StateT Game IO ()` where the game state is implicitly carried in
the `StateT` monad transformer. Additionally, I implemented the `StateT` myself
without looking at the implementation in `mtl` at all.

`ConnectFour.hs` contains the Connect Four game representation and a few helper
functions. The game board representation consists of two 64-bit words, one for
each player, where each bit describes one slot of the board. 1 means the slot
is occupied by the player in question.

Last but not least, `Negamax.hs` contains an implementation of the
[negamax](https://en.wikipedia.org/wiki/Negamax#Negamax_with_alpha_beta_pruning)
game algorithm which is used by the computer opponent. Although a bit redundant,
a data type for a game tree is defined and used there. There are also Functor
and Applicative instances for the GameTree datatype. Proofs for the applicative
functor laws are given in `gametree_proofs.txt`.

