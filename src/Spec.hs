import Test.Hspec
import Control.Exception (evaluate)
import Parser

-- Assert that the given SQL parses correctly, and that the same
-- parsed result is returned
assertSuccessfulParse :: String -> Assertion
assertSuccessfulParse sql = readExpr sql `shouldBe` sql

main :: IO ()
main = hspec $ do
    describe "Parsing and printing" $ do
        describe "Simple expresions" $ do
            it "supports select statements" $ do
                assertSuccessfulParse "select * from table"

            it "supports select statements with multiple selections" $ do
                assertSuccessfulParse "select table.foo, table.baz, table.qux from table"

            it "supports function calls" $ do
                assertSuccessfulParse "select count(*) from table"

            it "supports function calls with multiple arguments" $ do
                assertSuccessfulParse "select foo(a, b, c), bar(d, e, f) from table"
