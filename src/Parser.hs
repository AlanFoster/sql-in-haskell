-- This is a parser for a simplified version of SQL that doesn't support joins, or
-- multiple table lookups. For this reason there is no need for table.* syntax
--
-- Examples:
--
--      select * from table
--      select func(func(*), bar, baz), qux from table
--      select * from table where a >= 1 + 2 / 3
--      select * from table since monday at '8:00'
--      select * from table where col >= 1 + 2 / 3 since last week until yesterday at '08:00'
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
module Parser (readExpr) where

import Prelude hiding (until)
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
    | RelativeDateTime Day (Maybe Time)
    | ExactDateTime DateTime

instance Show Expression where
    show Star = "*"
    show (Number value) = show value
    show (Column identifier) = identifier
    show (Function name args) = name ++ "(" ++ joinWithCommas(args) ++ ")"
    show (BinaryOperator symbol left right) =
        "(" ++ (show left) ++ " " ++ (show symbol) ++ " " ++ (show right) ++ ")"
    show (RelativeDateTime day Nothing) = (show day)
    show (RelativeDateTime day (Just time)) = (show day) ++ " at '" ++ time ++ "'"
    show (ExactDateTime value) = "'" ++ value ++ "'"

type Time = String
type DateTime = String

data Day =
    Today
    | Yesterday
    | LastWeek
    | Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday

instance Show Day where
    show Today = "today"
    show Yesterday = "yesterday"
    show LastWeek = "last week"
    show Monday = "monday"
    show Tuesday = "tuesday"
    show Wednesday = "wednesday"
    show Thursday = "thursday"
    show Friday = "friday"
    show Saturday = "saturday"
    show Sunday = "sunday"

data Symbol =
    Plus
    | Minus
    | Multiply
    | Divide
    | Equal
    | NotEqual
    | GreaterThan
    | GreaterThanOrEqual
    | LessThan
    | LessThanOrEqual

instance Show Symbol where
    show Plus = "+"
    show Minus = "-"
    show Multiply = "*"
    show Divide = "/"
    show Equal = "="
    show NotEqual = "!="
    show GreaterThan = ">"
    show GreaterThanOrEqual = ">="
    show LessThan = "<"
    show LessThanOrEqual = "<="

-- select expressions from table where foo = 10
data Sql =
    Select {
        projection :: [Expression],
        table :: String,
        predicate :: Maybe Expression,
        since :: Maybe Expression,
        until :: Maybe Expression
    }

instance Show Sql where
    show select =
        "select " ++ (joinWithCommas $ projection select) ++ " " ++
        "from " ++ (table select) ++
        (maybe "" ((++) " where " . show) (predicate select)) ++
        (maybe "" ((++) " since " . show) (since select)) ++
        (maybe "" ((++) " until " . show) (until select))

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
        [
            Infix (parseBinary "*" Multiply) AssocLeft,
            Infix (parseBinary "/" Divide) AssocLeft
        ],

        -- Plus and Minus have the same precedence and are left associative
        [
            Infix (parseBinary "+" Plus) AssocLeft,
            Infix (parseBinary "-" Minus) AssocLeft
        ],

        -- Relational operators all have the same precedence, and I'm opting for it not being associative
        [
            Infix (parseBinary ">" GreaterThan) AssocNone,
            Infix (parseBinary ">=" GreaterThanOrEqual) AssocNone,

            Infix (parseBinary "<" LessThan) AssocNone,
            Infix (parseBinary "<=" LessThanOrEqual) AssocNone
        ],

        -- Equality Operators
        [
            Infix (parseBinary "=" Equal) AssocNone,
            Infix (parseBinary "!=" NotEqual) AssocNone
        ]
    ]

parseBinary :: String -> Symbol -> Parser (Expression -> Expression -> Expression)
parseBinary operatorString operatorSymbol =
    do
        void $ parseOperator operatorString
        spaces
        -- Partially apply our binary operator data constructor, the `buildExpressionParser`
        -- implementation will provide us with our left/right trees once parsing is successful
        return (BinaryOperator operatorSymbol)

-- Helper to help avoid ambiguity of operators by chosing the maximal munch
-- For instance, given an input string `>=` and a requiredOperator `>` without
-- this implementation we would consume a `>` and leave an invalid `=` character
-- behind. Instead we will only parse successfully if the required operator
-- consumes all characters
parseOperator :: String -> Parser String
parseOperator requiredOperator =  try $ withSpaces $ do
    maximalMunch <- many1 (oneOf "*/+->=<!")
    guard (maximalMunch == requiredOperator)
    return maximalMunch

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

parseString :: Parser String
parseString = withSpaces (char '\'' *> manyTill anyChar (char '\''))

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

parsePredicate :: Parser Expression
parsePredicate = parseExpression

parseRelativeDateTime :: Parser Expression
parseRelativeDateTime =
    RelativeDateTime
        <$> (
            (Today <$ (withSpaces $ try $ string "today")) <|>
            (Yesterday <$ (withSpaces $ try $ string "yesterday")) <|>
            (LastWeek <$ (withSpaces $ try $ string "last week")) <|>
            (Monday <$ (withSpaces $ try $ string "monday")) <|>
            (Tuesday <$ (withSpaces $ try $ string "tuesday")) <|>
            (Wednesday <$ (withSpaces $ try $ string "wednesday")) <|>
            (Thursday <$ (withSpaces $ try $ string "thursday")) <|>
            (Friday <$ (withSpaces $ try $ string "friday")) <|>
            (Saturday <$ (withSpaces $ try $ string "saturday")) <|>
            (Sunday <$ (withSpaces $ try $ string "sunday"))
        )
        <*> optionMaybe ((withSpaces $ string "at") *> parseString)

parseExactDateTime :: Parser Expression
parseExactDateTime = ExactDateTime <$> parseString

parseDateTime :: Parser Expression
parseDateTime =
    parseRelativeDateTime <|> parseExactDateTime

parseSelect :: Parser Sql
parseSelect =
    Select
        <$> ((withSpaces1 $ string "select") *> parseProjections)
        <*> ((withSpaces1 $ string "from") *> (withSpaces parseIdentifier))
        <*> optionMaybe ((withSpaces1 $ string "where") *> parsePredicate)
        <*> optionMaybe ((withSpaces1 $ string "since") *> parseDateTime)
        <*> optionMaybe ((withSpaces1 $ string "until") *> parseDateTime)

parseSql :: Parser Sql
parseSql =
    spaces *> (withSpaces parseSelect) <* eof

readExpr :: String -> String
readExpr input =
    case parse parseSql "sql" input of
        Left err -> "No match: " ++ show err
        Right val -> show val
