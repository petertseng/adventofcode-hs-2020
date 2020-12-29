# Advent of Code

[![Build Status](https://travis-ci.org/petertseng/adventofcode-hs-2020.svg?branch=master)](https://travis-ci.org/petertseng/adventofcode-hs-2020)

These are my solutions to http://adventofcode.com

All solutions are written in Haskell.

## Input

In general, all solutions can be invoked in both of the following ways:

* Without command-line arguments, takes input on standard input.
* With 1+ command-line arguments, reads input from the first, which must be the path to an input file.
  Arguments beyond the first are ignored.

Some may additionally support other ways:

Day 25 (Combo Breaker): Can also pass the public keys on ARGV

## Closing thoughts

You learn some cool tricks by being around other Haskellers.

Pairs:

```haskell
[(x, y) | x:ys <- tails xs, y <- ys]
```

Repeated permutations:

```haskell
replicateM 3 [0, -1, 1]
```
