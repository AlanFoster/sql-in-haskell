# SQL In Haskell

This repository represents a toy parser using the Parser Combinator library Parsec.
Currently the implementation echos back the SQL that you entered.

## Usage

This project uses Cabal:

```
cabal sandbox init

cabal configure
cabal install
cabal build
cabal run "select func(func(*), bar, baz), qux, * from table"
```

> http://katychuang.com/cabal-guide/
