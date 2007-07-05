module Main where

import Tokens
import Lexer
import Parser

import ScopeChecker
import SizeChecker

import System

main :: IO ()
main = do
  putStrLn $ "MiniAgda v0.0"
  args <- getArgs
  file <- readFile (args !! 0)
  let t = alexScanTokens file 
  let ast = parse t
  let ast2 = scopeCheck ast
  let b = sizeCheck ast2
  --putStrLn $ show ast
  putStrLn $ show ast2
  putStrLn $ show b