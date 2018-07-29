import Test.Hspec
import Control.Exception (evaluate)
import Parser

parsesAndReturns sql = readExpr sql `shouldBe` sql

main :: IO ()
main = hspec $ do
    describe "Parsing and printing" $ do
        describe "Simple expresions" $ do
            it "supports select statements" $ do
                parsesAndReturns "select * from table"

            it "supports select statements with multiple selections" $ do
                parsesAndReturns "select table.foo, table.baz, table.qux from table"

            it "supports function calls" $ do
                parsesAndReturns "select count(*) from table"

            it "supports function calls with multiple arguments" $ do
                parsesAndReturns "select foo(a, b, c), bar(d, e, f) from table"
