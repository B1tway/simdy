module Main (main) where

import Compiler (writeObject)
import Control.Monad ()
import Emit (buildIR, printIR)
import LLVM.Module

import Parser (parseToplevel)

import System.Environment ()

process :: String -> IO ()
process line =
    do
        let res = parseToplevel line
        let translate ex = do
                let ir = buildIR ex
                printIR ir
                writeObject (File ("output.o" :: FilePath)) ir
        either print translate res 

-- Right ex -> mapM_ print ex

main :: IO ()
main =
    do
        minput <- getContents
        putStrLn "AST: "
        process minput
