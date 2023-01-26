module Main (main)  where
import Parser (parseToplevel)
import Syntax ( Op(..), Expr(..), Type(..), PrimitiveType(..),Name, UnaryOp(..), MemoryOp(..) )
import Test.Tasty
import Test.Tasty.HUnit as H

testBinOp:: TestTree
testBinOp = 
    let
        input = "4 == 4"
        expr = [BinOp Equal (Number 4) (Number 4)]
    in 
        testCase "AST: parse expression 4 == 4" $ assertEqual [] True (expr == parseToplevel input)

tests :: TestTree
tests = testGroup "Unit tests: Approximation" 
        [ 
            testGroup "Test group: AST" [testBinOp]
        ]

main :: IO ()
main = defaultMain tests
