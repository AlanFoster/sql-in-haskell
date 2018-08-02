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

-- Abstract Data types

data Expression =
    Star
    | Number Integer
    | Column String
    | Function String [Expression]
    | BinaryOperator Symbol Expression Expression

instance Show Expression where
    show Star = "*"
    show (Number value) = show value
    show (Column identifier) = identifier
    show (Function name args) = name ++ "(" ++ joinWithCommas(args) ++ ")"
    show (BinaryOperator symbol left right) =
        "(" ++ (show left) ++ " " ++ (show symbol) ++ " " ++ (show right) ++ ")"

data Symbol = Plus | Minus | Multiply | Divide
instance Show Symbol where
    show Plus = "+"
    show Minus = "-"
    show Multiply = "*"
    show Divide = "/"

-- select expressions from table
data Sql = Select [Expression] String

instance Show Sql where
    show (Select selection table) =
        "select " ++ (joinWithCommas selection) ++ " from " ++ table

-- Helpers

joinWithCommas :: (Show a) => [a] -> String
joinWithCommas [] = ""
joinWithCommas [x] = show x
joinWithCommas (x:xs) = (show x) ++ ", " ++ (joinWithCommas xs)

withinBrackets :: Parser a -> Parser a
withinBrackets parser =
    (withSpaces $ char '(') *> parser <* (withSpaces $ char ')')

withSpaces :: Parser a -> Parser a
withSpaces p = p <* spaces

withSpaces1 :: Parser a -> Parser a
withSpaces1 p = p <* skipMany1 space

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` (withSpaces $ char ',')

-- Parsers

parseExpression :: Parser Expression
parseExpression = buildExpressionParser operatorTable parseTerm <?> "expression"

parseExpressions :: Parser [Expression]
parseExpressions = commaSep parseExpression

-- Specify the operator table and the associated precedences
-- Operators that appear first have higher precedence, operators that beside eachother
-- in the same array have the same precedence
operatorTable :: [[Operator Char () Expression]]
operatorTable = [
        -- Multiply and Divide have the same precedence and are left associative
        [Infix (parseBinary "*" Multiply) AssocLeft, Infix (parseBinary "/" Divide) AssocLeft],

        -- Plus and Minus have the same precedence and are left associative
        [Infix (parseBinary "+" Plus) AssocLeft, Infix (parseBinary "-" Minus) AssocLeft]
    ]

parseBinary :: String -> Symbol -> Parser (Expression -> Expression -> Expression)
parseBinary operatorString operatorSymbol =
    do
        void $ string operatorString
        spaces
        -- Partially apply our binary operator data constructor, the `buildExpressionParser`
        -- implementation will provide us with our left/right trees once parsing is successful
        return (BinaryOperator operatorSymbol)

parseTerm :: Parser Expression
parseTerm =
    (parseNumber
        <|> parseColumn
        <|> (withinBrackets parseExpression)
        <|> parseStar)
        <?> "term"

parseNumber :: Parser Expression
parseNumber = Number <$> parseInteger

parseInteger :: Parser Integer
parseInteger = liftM read (withSpaces $ many1 digit)

parseColumn :: Parser Expression
parseColumn = Column <$> parseIdentifier

parseIdentifier :: Parser String
parseIdentifier = withSpaces $ many1 letter

parseStar :: Parser Expression
parseStar = Star <$ (withSpaces $ char '*')

parseFunc :: Parser Expression
parseFunc =
    Function <$> (withSpaces parseIdentifier) <*> (withinBrackets parseExpressions)

parseProjection :: Parser Expression
parseProjection =
    (try $ withSpaces parseFunc) <|>
    (withSpaces parseStar) <|>
    parseExpression

parseProjections :: Parser [Expression]
parseProjections = commaSep parseProjection

parseSelect :: Parser Sql
parseSelect =
    Select
        <$> ((withSpaces1 $ string "select") *> parseProjections)
        <*> ((withSpaces1 $ string "from") *> (withSpaces parseIdentifier))

parseSql :: Parser Sql
parseSql =
    spaces *> (withSpaces parseSelect) <* eof

readExpr :: String -> String
readExpr input =
    case parse parseSql "sql" input of
        Left err -> "No match: " ++ show err
        Right val -> show val
