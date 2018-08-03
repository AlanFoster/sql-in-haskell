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
                            "No match: \"sql\" (line 1, column 36):\nunexpected '='\nexpecting space, operator, white space or end of input\nambiguous use of a non associative operator"

            describe "Complex predicates with precedence" $ do
                it "supports simple predicates with expressions" $ do
                    "select count(*) from table where foo = 10 + 2 / 3"
                        `shouldPrettyPrintAs`
                            "select count(*) from table where (foo = (10 + (2 / 3)))"

        describe "Edge cases" $ do
            it "Star syntax within brackets should work" $ do
                "select ((*)) from table"
                   `shouldPrettyPrintAs`
                    "select * from table"
