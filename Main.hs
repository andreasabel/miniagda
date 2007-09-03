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
  let ast2 = doScopeCheck ast
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

evalAllConst :: Signature -> Signature -> [(Name,Val)]
evalAllConst sig [] = []
evalAllConst sig ((n,def):xs) =
    case def of
      (ConstSig t e) -> 
          let ev = runEval sig emptyEnv e
          in case ev of
               Left err -> error $ "error during evaluation: " ++ err
               Right (v,_) -> (n,v):(evalAllConst sig xs)
      _ -> evalAllConst sig xs 


showAll :: Signature -> IO ()
showAll sig = let ls = map showConst (evalAllConst sig sig) in
                  sequence_ (map putStrLn ls)

showConst :: (Name,Val) -> String
showConst (n,v) = n ++ " evaluates to " ++ prettyVal v

termCheckAll :: [Declaration] -> IO ()
termCheckAll dl = do _ <- mapM terminationCheckDecl dl
                     return  ()

doTypeCheck :: [Declaration] -> IO Signature
doTypeCheck decl = do putStrLn $ "Typechecking ... "
                      let k = typeCheck decl
                      case k of
                        Left err -> error $ "error during typechecking: " ++ err
                        Right (_,sig) -> return $ sig

doScopeCheck :: [Declaration] -> [Declaration]
doScopeCheck decl = let k = scopeCheck decl 
                    in
                      case k of
                        Left err -> error $ "scope check error: " ++ err
                        Right (decl',_) -> decl'