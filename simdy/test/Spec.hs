module Main (main)  where
import Parser (parseToplevel)
import Syntax ( Op(..), Expr(..), Type(..), PrimitiveType(..),Name, UnaryOp(..), MemoryOp(..) )
import Test.Tasty
import Test.Tasty.HUnit as H

testVariableDef :: TestTree
testVariableDef = 
    let
        input = "var:ptr<u32>"
        expr = [DefVar "var" (Ptr (Primitive U32))]
    in 
        testCase "AST: parse expression var:ptr<u32>" $ assertEqual [] True (Right expr == parseToplevel input)


testBinOp :: TestTree
testBinOp = 
    let
        input = "4 == 4"
        expr = [BinOp Equal (Number 4) (Number 4)]
    in 
        testCase "AST: parse expression 4 == 4" $ assertEqual [] True (Right expr == parseToplevel input)

testFunc :: TestTree
testFunc = 
    let
        input = "func foo(arg1:u32, arg2:u16) {}"
        expr = [Function "foo" [DefVar "arg1" (Primitive U32), DefVar "arg2" (Primitive U16) ] []]
    in 
        testCase "AST: parse expression func foo(arg1:u32, arg2:u16) {}" $ assertEqual [] True (Right expr == parseToplevel input)

testCall :: TestTree
testCall =  
    let
        input = "call(arg1, arg2)"
        expr = [Call "call" [Variable "arg1", Variable "arg2"]]
    in 
        testCase "AST: parse expression call(arg1, arg2)" $ assertEqual [] True (Right expr == parseToplevel input)

testIndexOp :: TestTree
testIndexOp = 
    let
        input = "arr[2]"
        expr = [IndexOp (Variable "arr") (Number 2)]
    in 
        testCase "AST: parse expression arr[2]" $ assertEqual [] True (Right expr == parseToplevel input)

testIf :: TestTree
testIf = 
    let
        input = "if (var1 > var2 ) {var1=var2} else {var2=var1}"
        expr = [If (BinOp Greater (Variable "var1") (Variable "var2")) [BinOp Assign (Variable "var1") (Variable "var2")] [BinOp Assign (Variable "var2") (Variable "var1")]]
    in 
        testCase "AST: parse expression if (var1 > var2 ) {var1=var2} else {var2=var1}" $ assertEqual [] True (Right expr == parseToplevel input)

testWhile :: TestTree
testWhile = 
    let
        input = "while (var < 10) {++var}"
        expr = [While (BinOp Less (Variable "var") (Number 10)) [UnOp Increment (Variable "var")]]
    in 
        testCase "AST: parse expression while (var < 10) {++var}" $ assertEqual [] True (Right expr == parseToplevel input)

testCastOp :: TestTree
testCastOp =
    let
        input = "bitcast<u32>var"
        expr = [CastOp (Primitive U32) (Variable "var")]
    in 
        testCase "AST: parse expression bitcast<u32>var" $ assertEqual [] True (Right expr == parseToplevel input)

testMemOp :: TestTree
testMemOp =
    let
        input = 
            "addr:ptr<u32> store<u32>(addr, 0)"
        expr = [DefVar "addr" (Ptr (Primitive U32)),MemOp (Store (Primitive U32) (Variable "addr") (Number 0))]
    in 
        testCase "AST: parse expression addr:ptr<u32> = 2 store<u32>(addr, 0)" $ assertEqual [] True (Right expr == parseToplevel input)


tests :: TestTree
tests = testGroup "Unit tests: Approximation" 
        [ 
            testGroup "Test group: AST" [testVariableDef, testBinOp, testFunc, testCall, testIndexOp, testIf, testWhile, testCastOp, testMemOp]
        ]

main :: IO ()
main = defaultMain tests
