module Main (main) where

import Parser (parseToplevel)

import System.Environment()
import Control.Monad()
import GHC.Exts (build)
import Emit (buildIR, printIR)
process :: String -> IO ()
process line = do
  let res = parseToplevel line
  case res of
    Left err -> print err
    Right ex -> printIR ir
      where ir = buildIR ex
    -- Right ex -> mapM_ print ex

main :: IO ()
main =  do
    minput <- getContents
    putStrLn "AST: "
    process minput