module Main (main) where

import Parser (parseToplevel)
import LLVM.Module 

import System.Environment()
import Control.Monad()
import Emit (buildIR, printIR)
import Compiler (writeObject)


process :: String -> IO ()
process line = do
  let res = parseToplevel line
  case res of
    Left err -> print err
    Right ex -> 
      do 
        printIR ir
        writeObject (File ("output.o" :: FilePath)) ir
      where ir = buildIR ex
    -- Right ex -> mapM_ print ex

main:: IO()
main =  do
    minput <- getContents
    putStrLn "AST: "
    process minput