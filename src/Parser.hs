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

data Projection =
    Star
    | ProjectionExpression ProjectionExpression
    | Column String
    | TableColumn String String
    | Function String [Projection]

instance Show Projection where
    show Star = "*"
    show (Column column) = column
    show (TableColumn table column) = table ++ "." ++ column
    show (Function name args) = name ++ "(" ++ joinWithCommas(args) ++ ")"
    show (ProjectionExpression e) = show e

data Symbol = Plus | Minus | Multiply | Divide

instance Show Symbol where
    show Plus = "+"
    show Minus = "-"
    show Multiply = "*"
    show Divide = "/"

data ProjectionExpression =
    Number Integer
    | BinaryOperator Symbol ProjectionExpression ProjectionExpression

instance Show ProjectionExpression where
    show (Number value) =
        show value
    show (BinaryOperator symbol left right) =
        "(" ++ (show left) ++ " " ++ (show symbol) ++ " " ++ (show right) ++ ")"

data Table =
    Table String

instance Show Table where
    show (Table table) = table

data Sql =
    Select [Projection] Table

instance Show Sql where
    show (Select selection table) =
        "select " ++ (joinWithCommas selection) ++ " from " ++ (show table)

parseProjectionExpr :: Parser ProjectionExpression
parseProjectionExpr = buildExpressionParser operatorTable parseTerm <?> "expression"

-- Specify the operator table and the associated precedences
-- Operators that appear first have higher precedence, operators that beside eachother
-- in the same array have the same precedence
operatorTable = [
        -- Multiply and Divide have the same precedence and are left associative
        [Infix (parseBinary "*" Multiply) AssocLeft, Infix (parseBinary "/" Divide) AssocLeft],

        -- Plus and Minus have the same precedence and are left associative
        [Infix (parseBinary "+" Plus) AssocLeft, Infix (parseBinary "-" Minus) AssocLeft]
    ]

parseBinary :: String -> Symbol -> Parser (ProjectionExpression -> ProjectionExpression -> ProjectionExpression)
parseBinary operatorString operatorSymbol =
    do
        string operatorString
        spaces
        -- Partially apply our binary operator data constructor, the `buildExpressionParser`
        -- implementation will provide us with our left/right trees once parsing is successful
        return (BinaryOperator operatorSymbol)

withBrackets :: Parser a -> Parser a
withBrackets parser =
    do
        char '('
        result <- parser
        char ')'
        return result

parseTerm :: Parser ProjectionExpression
parseTerm =
    do
        term <- parseNumber <|> (withBrackets parseProjectionExpr)
        spaces
        return term <?> "term"

parseNumber :: Parser ProjectionExpression
parseNumber = liftM (Number . read) $ many1 digit

spaces1 :: Parser ()
spaces1 = skipMany1 space

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p (char ',' >> spaces)

parseName :: Parser String
parseName = many1 letter

parseStar :: Parser Projection
parseStar = char '*' >> return Star

parseTableColumn :: Parser Projection
parseTableColumn =
    do
        table <- parseName
        char '.'
        column <- (string "*" <|> parseName)
        return $ TableColumn table column

parseColumn :: Parser Projection
parseColumn =
    do
        column <- parseName
        return $ Column column

parseColumnProjection :: Parser Projection
parseColumnProjection =
    try parseTableColumn <|>
    parseColumn

parseFunc :: Parser Projection
parseFunc =
    do
        name <- parseName
        args <- (withBrackets parseProjections)
        return $ Function name args

parseProjection :: Parser Projection
parseProjection =
    (parseProjectionExpr >>= (\x -> return (ProjectionExpression x))) <|>
    parseStar <|>
    try parseFunc <|>
    parseColumnProjection

parseProjections :: Parser [Projection]
parseProjections = commaSep parseProjection

parseSelect :: Parser Sql
parseSelect =
    do
        spaces
        string "select"
        spaces1
        projections <- parseProjections
        spaces
        string "from"
        spaces1
        table <- parseName
        spaces
        eof
        return (Select projections (Table table))

parseSql :: Parser Sql
parseSql =
    parseSelect

readExpr :: String -> String
readExpr input =
    case parse parseSql "sql" input of
        Left err -> "No match: " ++ show err
        Right val -> show val
