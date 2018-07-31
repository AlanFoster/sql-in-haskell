-- Parser for a simple SQL like language:
--     select * from table
--     select table.* from table
--     select table.a from table
--     select a, b, c from table
--     select a from table
--     select func(*), func(*) from table
--     select func(func(*)), func(*) from table
--     select [expr]* from table
module Parser (readExpr) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Control.Monad

joinWithCommas :: (Show a) => [a] -> String
joinWithCommas [] = ""
joinWithCommas [x] = show x
joinWithCommas (x:xs) = (show x) ++ ", " ++ (joinWithCommas xs)

data Argument =
    Star
    | ArgumentExpression Expression
    | Column String
    | TableColumn String String
    | Function String [Argument]

instance Show Argument where
    show Star = "*"
    show (Column column) = column
    show (TableColumn table column) = table ++ "." ++ column
    show (Function name args) = name ++ "(" ++ joinWithCommas(args) ++ ")"
    show (ArgumentExpression e) = show e

data Symbol = Plus | Minus

instance Show Symbol where
    show Plus = "+"
    show Minus = "-"

data Expression =
    Number Integer
    | BinaryOperator Symbol Expression Expression

instance Show Expression where
    show (Number value) =
        show value
    show (BinaryOperator symbol left right) =
        "(" ++ (show left) ++ " " ++ (show symbol) ++ " " ++ (show right) ++ ")"

data Table =
    Table String

instance Show Table where
    show (Table table) = table

data Sql =
    Select [Argument] Table

instance Show Sql where
    show (Select selection table) =
        "select " ++ (joinWithCommas selection) ++ " from " ++ (show table)

parseExpr :: Parser Expression
parseExpr = buildExpressionParser operatorTable parseTerm <?> "expression"

-- Specify the operator table and the associated precedences
-- Operators that appear first have higher precedence, operators that beside eachother
-- in the same array have the same precedence
operatorTable = [
        -- Plus and Minus have the same precedence and are left associative
        [Infix (parseBinary "+" Plus) AssocLeft, Infix (parseBinary "-" Minus) AssocLeft]
    ]

parseBinary :: String -> Symbol -> Parser (Expression -> Expression -> Expression)
parseBinary operatorString operatorSymbol =
    do
        string operatorString
        spaces
        -- Partially apply our binary operator data constructor, the `buildExpressionParser`
        -- implementation will provide us with our left/right trees once parsing is successful
        return (BinaryOperator operatorSymbol)

parseTerm :: Parser Expression
parseTerm =
    do
        number <- parseNumber
        spaces
        return number <?> "number"

parseNumber :: Parser Expression
parseNumber = liftM (Number . read) $ many1 digit

spaces1 :: Parser ()
spaces1 = skipMany1 space

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p (char ',' >> spaces)

parseName :: Parser String
parseName = many1 letter

parseStar :: Parser Argument
parseStar = char '*' >> return Star

parseTableColumn :: Parser Argument
parseTableColumn =
    do
        table <- parseName
        char '.'
        column <- (string "*" <|> parseName)
        return $ TableColumn table column

parseColumn :: Parser Argument
parseColumn =
    do
        column <- parseName
        return $ Column column

parseColumnArgument :: Parser Argument
parseColumnArgument =
    try parseTableColumn <|>
    parseColumn

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
    (parseExpr >>= (\x -> return (ArgumentExpression x))) <|>
    parseStar <|>
    try parseFunc <|>
    parseColumnArgument

parseArgs :: Parser [Argument]
parseArgs = commaSep parseArg

parseSelect :: Parser Sql
parseSelect =
    do
        spaces
        string "select"
        spaces1
        args <- parseArgs
        spaces
        string "from"
        spaces1
        table <- parseName
        spaces
        eof
        return (Select args (Table table))

parseSql :: Parser Sql
parseSql =
    parseSelect

readExpr :: String -> String
readExpr input =
    case parse parseSql "sql" input of
        Left err -> "No match: " ++ show err
        Right val -> show val
