![Logo of the Advent of Code, an ASCII christmas tree](https://aplwiki.com/images/0/0d/Advent_Of_Code_Logo.png)

# Introduction

This is my solution for the [Advent Of Code 2025](https://adventofcode.com/2025/about).

I wanted to challenge myself this year and learn a new language: [Haskell](https://www.haskell.org/)

# About Haskell

Haskell is a programming language focused on [Functional Programming](https://en.wikipedia.org/wiki/Functional_programming). By design, it emphasizes declarative, statically typed code.

It features type safety, immutability, lazy evaluation, referential transparency and powerful abstractions.

> Concepts that will blow your mind — relearn programming while having an absolute blast.

# File structure

Each solution is contained within a folder for the date, such as `day1/`, `day2/`, etc. The file to execute for each solution is called `Main.hs`.

# How to run

If this is your first time running Haskell, you can use this simple installer to get started: [GHCup](https://www.haskell.org/ghcup/). You may be asked to install tools for compiling C code on your platform. This is needed as haskell generates C code.

Once it is installed, you can run the solution with

```sh
$ cabal run day1
Hello World!
```

# Hot-reload

You can also run the solution interactively with `ghcid`.

```sh
cabal install ghcid
```

This will automatically reload every time you change the code. The main method will also be executed automatically allowing you to run tests.

```sh
$ cd day1
$ ghcid
```

Additionally, you can use eval comments to check the result of your functions as you go

```hs
-- Capitalizes the first letter of a string
capitalize :: String -> String
capitalize (x : xs) = toUpper x : xs
capitalize [] = []
-- $> capitalize "hello"
```

```sh
$ ghcid
$> capitalize "hello"
"Hello"
```

