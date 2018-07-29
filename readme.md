# SQL In Haskell

This repository represents a toy parser using the Parser Combinator library Parsec.
Currently the implementation echos back the SQL that you entered.

## Usage

This project uses [Cabal](http://katychuang.com/cabal-guide/):
Ensure you have a cabal sandbox up and running:

```shell
cabal sandbox init

cabal configure
cabal install
cabal build
cabal run "select func(func(*), bar, baz), qux, * from table"
```

## Tests

To run the [hspec](https://hspec.github.io/) tests, use cabal:

```shell
cabal test
```
