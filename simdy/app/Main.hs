module Main (main) where
import Compiler (writeObject)
import Control.Monad ()
import Emit (buildIR, printIR)
import LLVM.Module

import Parser (parseToplevel)

import System.Environment ()

process :: String -> IO ()
process line
  = do let res = parseToplevel line
       case res of
           Left err -> print err
           Right ex -> do printIR ir
                          writeObject (File ("output.o" :: FilePath)) ir
             where ir = buildIR ex

-- Right ex -> mapM_ print ex

main :: IO ()
main
  = do minput <- getContents
       putStrLn "AST: "
       process minput
