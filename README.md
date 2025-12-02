# AOC2025

Advent of Code 2025 solutions in Haskell


### Installation

This is a stack project, so you can probably compile and run using:

    stack build

and:

    stack run -- AOC2025-exe


I would recommend using ghcup to install the required verions of stack, ghc, cabal, etc. I call it using:

    ghcup tui



### Profiling

stack build --profile

stack exec --profile -- AOC2025-exe +RTS -p

