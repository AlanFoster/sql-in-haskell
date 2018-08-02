import Test.Hspec
import Parser

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

        describe "Edge cases" $ do
            it "Star syntax within brackets should work" $ do
                "select ((*)) from table"
                   `shouldPrettyPrintAs`
                    "select * from table"
