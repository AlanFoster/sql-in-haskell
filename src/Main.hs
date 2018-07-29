module Main where

import System.Environment
import Parser

main :: IO ()
main =
    do
        args <- getArgs
        print $ readExpr (args !! 0)
