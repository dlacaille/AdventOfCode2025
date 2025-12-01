![Logo of the Advent of Code, an ASCII christmas tree](https://aplwiki.com/images/0/0d/Advent_Of_Code_Logo.png)

# Introduction

This is my solution for the [Advent Of Code 2025](https://adventofcode.com/2025/about).

I wanted to challenge myself this year and learn a new language: [Haskell](https://www.haskell.org/)

# About Haskell

Haskell is a [Functional Programming](https://en.wikipedia.org/wiki/Functional_programming) language. By design, it emphasizes declarative, statically typed code.

It features type safety, immutability, lazy evaluation, referential transparency and powerful abstractions.

> Concepts that will blow your mind — relearn programming while having an absolute blast.

# File structure

Each solution is contained within a folder for the date, such as `day01/`, `day02/`, etc.

For simplicity's sake, the solution is named `Solution.hs` and is structured like a Library so that it can be tested easily. This way I can quickly copy a day's folder to the next one to get started.

A `test/Spec.hs` file is used for unit tests of all solution functions. `pendingWith` is used to output the result of the puzzles without having to assert it. This was easier than defining a way to execute each puzzle and makes it easier to run unit tests every time the solution is changed (See [Hot-reload](#hot-reload)).

# How to run

If this is your first time running Haskell, you can use this simple installer to get started: [GHCup](https://www.haskell.org/ghcup/). You may be asked to install tools for compiling C code on your platform. This is needed as haskell generates C code.

Once it is installed, you can run the solution with

```sh
$ cd day1
$ cabal run test
parseLine
  parses line correctly [✔]
rotateDial
  rotates left correctly [✔]
  rotates right correctly [✔]
  handles invalid direction [✔]
  handles zero rotation [✔]
  handles full rotation [✔]
countTimesPassedZero
  counts ending on zero when rotating left [✔]
  counts passing over zero when rotating left [✔]
  counts ending on zero when rotating right [✔]
  counts passing over zero when rotating right [✔]
  handles no passes over zero when rotating left [✔]
  handles no passes over zero when rotating right [✔]
  does not count starting on zero [✔]
  handles passing over 0 multiple times [✔]
puzzle1
  computes correct result for sample input [✔]
  processes input file [‐]
    # PENDING: 1048
puzzle2
  computes correct result for sample input [✔]
  processes input file [‐]
    # PENDING: 6498

Finished in 0.0076 seconds
18 examples, 0 failures, 2 pending

...done
```

# Hot-reload

You can also run the solution interactively with `ghcid`.

```sh
cabal install ghcid
```

This will automatically reload every time you change the code. The tests will also be executed automatically as you develop.

```sh
$ cd day1
$ ghcid
```
