module Main where

import Tokens
import Lexer
import Parser

import Abstract
import ScopeChecker
import SizeChecker
import TypeChecker
import Value
import Signature
import TermCheck2

import System

main :: IO ()
main = do
  putStrLn $ "MiniAgda v0.0"
  args <- getArgs
  file <- readFile (args !! 0)
  let t = alexScanTokens file 
  let ast = parse t
  let ast2 = scopeCheck ast
  -- let b = sizeCheck ast2
  sig <- doTypeCheck ast2
  -- putStrLn $ show ast
  -- putStrLn $ show ast2
  -- putStrLn $ show b
  -- putStrLn "Signature:"
  -- putStrLn $ show sig
  _ <- termCheckAll ast2
  showAll sig
  return ()
--evaluate all constants

evalAll :: Signature -> Signature -> [(Name,Val)]
evalAll sig [] = []
evalAll sig ((n,def):xs) = case def of
                        (ConstSig t e) -> (n,runEval sig emptyEnv e):(evalAll sig xs)
                        _ -> evalAll sig xs 


showAll :: Signature -> IO ()
showAll sig = let ls = map showConst (evalAll sig sig) in
                  sequence_ (map putStrLn ls)

showConst :: (Name,Val) -> String
showConst (n,v) = n ++ " evaluates to " ++ prettyVal v

termCheckAll :: [Declaration] -> IO ()
termCheckAll dl = do _ <- mapM terminationCheckDecl dl
                     return  ()

doTypeCheck :: [Declaration] -> IO Signature
doTypeCheck decl = do putStrLn $ "Typechecking ... "
                      let sig = typeCheck decl
                      _ <- if (length sig > 0) then 
                               putStrLn $ "TypeChecking ok" 
                           else return ()
                      return sig