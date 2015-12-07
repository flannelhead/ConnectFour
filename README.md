# ConnectFour
A small implementation of Connect Four game with beautiful ANSI terminal
graphics and a computer opponent. This project was started on a university 
programming course and has proven to be a great way to learn more about
Haskell's powerful abstractions.

## Structure
`Main.hs` contains most of the gameplay and console drawing logic.

`ConnectFour.hs` contains the Connect Four game representation and a few helper
functions. The game board representation consists of two 64-bit words, one for
each player, where each bit describes one slot of the board. 1 means the slot
is occupied by the player in question.

Last but not least, `Negamax.hs` contains an implementation of the
[negamax](https://en.wikipedia.org/wiki/Negamax#Negamax_with_alpha_beta_pruning)
game algorithm which is used by the computer opponent. The algorithm is a work in
progress. The intent is to generalize it using Haskell's abstractions.

