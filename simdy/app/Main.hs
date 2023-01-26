module Main (main) where

import Parser (parseToplevel)
import qualified Data.Text.Lazy.IO as TLIO

import LLVM.Module 

import System.IO
import System.Environment()
import Control.Monad()
import GHC.Exts (build)
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


main =  do
    minput <- getContents
    putStrLn "AST: "
    process minput