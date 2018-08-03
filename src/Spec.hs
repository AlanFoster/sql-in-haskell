import Test.Hspec
import Parser
import Control.Monad

shouldPrettyPrintAs :: String -> String -> IO ()
shouldPrettyPrintAs sql expected = readExpr sql `shouldBe` expected

main :: IO ()
main = hspec $ do
    describe "Parsing and printing" $ do
        describe "Simple expresions" $ do
            it "supports select statements" $ do
                let sql = "select * from table"
                sql `shouldPrettyPrintAs` sql

            it "supports select statements with multiple selections" $ do
                let sql = "select foo, baz, qux from table"
                sql `shouldPrettyPrintAs` sql

            it "supports function calls" $ do
                let sql = "select count(*) from table"
                sql `shouldPrettyPrintAs` sql

            it "supports function calls with multiple arguments" $ do
                let sql = "select foo(a, b, c), bar(d, e, f) from table"
                sql `shouldPrettyPrintAs` sql

            it "supports function calls with multiple arguments and white space" $ do
                "      select foo  (  a  ,   b   ,   c  )  ,   bar  (   d   ,    e   ,   f,   *   )    from   foo    "
                    `shouldPrettyPrintAs`
                    "select foo(a, b, c), bar(d, e, f, *) from foo"

        describe "it provides supports operator precedence" $ do
            it "supports the addition of numbers" $ do
                "select 1 + 2 from table"
                    `shouldPrettyPrintAs`
                    "select (1 + 2) from table"

            it "supports the addition of numbers" $ do
                "select 1 + 2 + 3 from table"
                    `shouldPrettyPrintAs`
                    "select ((1 + 2) + 3) from table"

            it "supports the addition of numbers" $ do
                "select 1 + 2 + 3 + 4 from table"
                   `shouldPrettyPrintAs`
                    "select (((1 + 2) + 3) + 4) from table"

            it "supports the subtraction of numbers" $ do
                "select 1 - 2 from table"
                    `shouldPrettyPrintAs`
                    "select (1 - 2) from table"

            it "supports an arbitrary selection of operators" $ do
                "select 1 + 2 - 3 + 4 from table"
                   `shouldPrettyPrintAs`
                    "select (((1 + 2) - 3) + 4) from table"

            it "supports an arbitrary selection of operators" $ do
                "select max(1 + 2 * 3 / 2 + (1 - 1)), min(1 + 2) from table"
                   `shouldPrettyPrintAs`
                    "select max(((1 + ((2 * 3) / 2)) + (1 - 1))), min((1 + 2)) from table"

        describe "predicates" $ do
            describe "Relation and equality operators" $ do
                let operatorTests = [
                            -- Relational Operators
                            ("select * from table where foo < 10", "select * from table where (foo < 10)"),
                            ("select * from table where foo <= 10", "select * from table where (foo <= 10)"),
                            ("select * from table where foo > 10", "select * from table where (foo > 10)"),
                            ("select * from table where foo >= 10", "select * from table where (foo >= 10)"),

                            -- Equality Operators
                            ("select * from table where foo = 10", "select * from table where (foo = 10)"),
                            ("select * from table where foo != 10", "select * from table where (foo != 10)")
                        ]

                forM_ operatorTests $ \(input, expected) ->
                    it (show input ++ " pretty print as " ++ show expected) $ do
                        input `shouldPrettyPrintAs` expected

                it "Shouldn't parse multiple equality operators beside eachother" $ do
                    "select * from table where foo = 10 = 20"
                        `shouldPrettyPrintAs`
                            "No match: \"sql\" (line 1, column 36):\nunexpected '='\nexpecting space, operator, white space, \"since\", \"until\" or end of input\nambiguous use of a non associative operator"

            describe "Complex predicates with precedence" $ do
                it "supports simple predicates with expressions" $ do
                    "select count(*) from table where foo = 10 + 2 / 3"
                        `shouldPrettyPrintAs`
                            "select count(*) from table where (foo = (10 + (2 / 3)))"

        describe "date handling" $ do
            describe "Since logic" $ do
                let dateTests = [
                            -- Relative date only
                            "select * from table since today",
                            "select * from table since yesterday",
                            "select * from table since last week",
                            "select * from table since monday",
                            "select * from table since tuesday",
                            "select * from table since wednesday",
                            "select * from table since thursday",
                            "select * from table since friday",
                            "select * from table since saturday",
                            "select * from table since sunday",

                            -- Relative date and time
                            "select * from table since today at '8:00'",
                            "select * from table since yesterday at '8:00'",
                            "select * from table since last week at '8:00'",
                            "select * from table since monday at '8:00'",
                            "select * from table since tuesday at '8:00'",
                            "select * from table since wednesday at '8:00'",
                            "select * from table since thursday at '8:00'",
                            "select * from table since friday at '8:00'",
                            "select * from table since saturday at '8:00'",
                            "select * from table since sunday at '8:00'",

                            -- Specific times
                            "select * from table since '2018-03-08'",
                            "select * from table since '2018-03-08 08:00'",
                            "select * from table since '2018-03-08 08:00:00'",
                            "select * from table since '08:00'"
                        ]

                forM_ dateTests $ \input ->
                    it (show input ++ " pretty print as " ++ show input) $ do
                        input `shouldPrettyPrintAs` input

            describe "until logic" $ do
                let dateTests = [
                            -- Relative date only
                            "select * from table until today",
                            "select * from table until yesterday",
                            "select * from table until last week",
                            "select * from table until monday",
                            "select * from table until tuesday",
                            "select * from table until wednesday",
                            "select * from table until thursday",
                            "select * from table until friday",
                            "select * from table until saturday",
                            "select * from table until sunday",

                            -- Relative date and time
                            "select * from table until today at '8:00'",
                            "select * from table until yesterday at '8:00'",
                            "select * from table until last week at '8:00'",
                            "select * from table until monday at '8:00'",
                            "select * from table until tuesday at '8:00'",
                            "select * from table until wednesday at '8:00'",
                            "select * from table until thursday at '8:00'",
                            "select * from table until friday at '8:00'",
                            "select * from table until saturday at '8:00'",
                            "select * from table until sunday at '8:00'",

                            -- Specific times
                            "select * from table until '2018-03-08'",
                            "select * from table until '2018-03-08 08:00'",
                            "select * from table until '2018-03-08 08:00:00'",
                            "select * from table until '08:00'"
                        ]

                forM_ dateTests $ \input ->
                    it (show input ++ " pretty print as " ++ show input) $ do
                        input `shouldPrettyPrintAs` input

        describe "Edge cases" $ do
            it "Star syntax within brackets should work" $ do
                "select ((*)) from table"
                   `shouldPrettyPrintAs`
                    "select * from table"
