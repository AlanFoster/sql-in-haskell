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
```

Entering an interactive terminal with the parsing library loaded, which is great for exploring
parsing:

```
cabal repl scheme-in-haskell
```

Running the program directly:

```
cabal run "select func(func(*), bar, baz), qux, * from table"
```

## Tests

To run the [hspec](https://hspec.github.io/) tests, use cabal:

```shell
cabal test
```

## Useful Resources

- http://learnyouahaskell.com/
- http://dev.stephendiehl.com/hask/
- http://blog.moertel.com/posts/2005-08-27-power-parsing-with-haskell-and-parsec.html
- https://kseo.github.io/posts/2014-01-07-parsing-arithmetic-expressions-with-parsec.html
- https://wiki.haskell.org/Parsing_expressions_and_statements
