-- Parser for a simple SQL like language:
--     select * from table
--     select a, b, c from table
--     select a from table
--     select func(*), func(*) from table
--     select func(func(*)), func(*) from table
--     select [expr]* from table
--
-- Note this is a simplified version of SQL that doesn't support joins, or
-- multiple table lookups. For this reason there is no need for table.* syntax
--
-- Useful notes /  Hoogle cheat sheet:
--
--  <|> Represents the choice operator
--  <$> Infix synonym for fmap
--          (<$>) :: Functor f => (a -> b) -> f a -> f b
--  <*> Sequential application, given a Functor containing a function
--          (<*>) :: f (a -> b) -> f a -> f b
--  <* Sequence actions, discarding the result of the second argument
--          (<*) :: f a -> f b -> f a
--  *> Sequence actions, discarding the result of the first argument
--          (*>) :: f a -> f b -> f b
--  ap
--            ap :: Monad m => m (a -> b) -> m a -> m b
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
    | Function String [Projection]

instance Show Projection where
    show Star = "*"
    show (Column column) = column
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
operatorTable :: [[Operator Char () ProjectionExpression]]
operatorTable = [
        -- Multiply and Divide have the same precedence and are left associative
        [Infix (parseBinary "*" Multiply) AssocLeft, Infix (parseBinary "/" Divide) AssocLeft],

        -- Plus and Minus have the same precedence and are left associative
        [Infix (parseBinary "+" Plus) AssocLeft, Infix (parseBinary "-" Minus) AssocLeft]
    ]

parseBinary :: String -> Symbol -> Parser (ProjectionExpression -> ProjectionExpression -> ProjectionExpression)
parseBinary operatorString operatorSymbol =
    do
        void $ string operatorString
        spaces
        -- Partially apply our binary operator data constructor, the `buildExpressionParser`
        -- implementation will provide us with our left/right trees once parsing is successful
        return (BinaryOperator operatorSymbol)

withinBrackets :: Parser a -> Parser a
withinBrackets parser =
    (withSpaces $ char '(') *> parser <* (withSpaces $ char ')')

parseTerm :: Parser ProjectionExpression
parseTerm =
    withSpaces (parseNumber <|> (withinBrackets parseProjectionExpr)) <?> "term"

parseNumber :: Parser ProjectionExpression
parseNumber = liftM (Number . read) $ many1 digit

spaces1 :: Parser ()
spaces1 = skipMany1 space

withSpaces :: Parser a -> Parser a
withSpaces p = p <* spaces

withSpaces1 :: Parser a -> Parser a
withSpaces1 p = p <* spaces1

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` (withSpaces $ char ',')

parseName :: Parser String
parseName = many1 letter

parseStar :: Parser Projection
parseStar = char '*' *> return Star

parseColumn :: Parser Projection
parseColumn = Column <$> parseName

parseFunc :: Parser Projection
parseFunc =
    Function <$> (withSpaces parseName) <*> (withinBrackets parseProjections)

parseProjection :: Parser Projection
parseProjection =
    (ProjectionExpression <$> parseProjectionExpr) <|>
    (withSpaces parseStar) <|>
    (try $ withSpaces parseFunc) <|>
    (withSpaces parseColumn)

parseProjections :: Parser [Projection]
parseProjections = commaSep parseProjection

parseSelect :: Parser Sql
parseSelect =
    Select
        <$> ((withSpaces1 $ string "select") *> parseProjections)
        <*> ((withSpaces1 $ string "from") *> (Table <$> (withSpaces parseName)))

parseSql :: Parser Sql
parseSql =
    spaces *> (withSpaces parseSelect) <* eof

readExpr :: String -> String
readExpr input =
    case parse parseSql "sql" input of
        Left err -> "No match: " ++ show err
        Right val -> show val
