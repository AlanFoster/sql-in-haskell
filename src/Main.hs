module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

-- Parser for a simple SQL like language:
--     select * from table
--     select a, b, c from table
--     select a from table
--     select func(*), func(*) from table
--     select func(func(*)), func(*) from table
--     select [expr]* from table

joinWithCommas :: (Show a) => [a] -> String
joinWithCommas [] = ""
joinWithCommas [x] = show x
joinWithCommas (x:xs) = (show x) ++ ", " ++ (joinWithCommas xs)

data Argument =
    Star
    | Column String
    | Function String [Argument]

instance Show Argument where
    show (Column column) = column
    show Star = "*"
    show (Function name args) = name ++ "(" ++ joinWithCommas(args) ++ ")"

data Table =
    Table String

instance Show Table where
    show (Table table) = table

data Sql =
    Select [Argument] Table

instance Show Sql where
    show (Select selection table) =
        "select " ++ (joinWithCommas selection) ++ " from " ++ (show table)

spaces :: Parser ()
spaces = skipMany1 space

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p (char ',' >> skipMany spaces)

parseName :: Parser String
parseName = many1 letter

parseStar :: Parser Argument
parseStar = char '*' >> return Star

parseColumn :: Parser Argument
parseColumn =
    do
        name <- parseName
        return $ Column name

parseFunc :: Parser Argument
parseFunc =
    do
        name <- parseName
        char '('
        args <- parseArgs
        char ')'
        return $ Function name args

parseArg :: Parser Argument
parseArg =
    parseStar <|>
    try parseFunc <|>
    parseColumn

parseArgs :: Parser [Argument]
parseArgs = commaSep parseArg

parseSelect :: Parser Sql
parseSelect =
    do
        string "select"
        spaces
        args <- parseArgs
        spaces
        string "from"
        spaces
        table <- parseName
        return (Select args (Table table))

parseExpr :: Parser Sql
parseExpr =
    parseSelect

readExpr :: String -> String
readExpr input =
    case parse parseExpr "sql" input of
        Left err -> "No match: " ++ show err
        Right val -> show val

main :: IO ()
main =
    do
        args <- getArgs
        print $ readExpr (args !! 0)
