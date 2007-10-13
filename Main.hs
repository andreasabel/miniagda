module Main where

import Tokens
import Lexer
import Parser

import Abstract
import ScopeChecker
import TypeChecker
import Value
import TermCheck2

import System

main :: IO ()
main = do
  putStrLn $ "***** MiniAgda v1.0 *****"
  args <- getArgs
  file <- readFile (args !! 0)
  t <- return $ alexScanTokens file 
  ast <- return $ parse t
  putStrLn ("--- scope checking ---")
  ast2 <- return $ doScopeCheck ast
  putStrLn ("--- termination checking ---")
  _ <- termCheckAll ast2  
  putStrLn ("--- type checking ---")
  tc <- doTypeCheck ast2
  case tc of
    Nothing -> return ()
    Just sig -> do putStrLn "--- evaluating constants ---" 
                   showAll sig ast2
  

--evaluate all constants
evalAllConst :: Signature -> [Declaration] -> [(Name,Val)]
evalAllConst sig [] = []
evalAllConst sig (decl:xs) =
    case decl of
      (ConstDecl True (TypeSig n t) e) -> 
          let ev =  runEval sig e
          in case ev of
               Left err -> error $ "error during evaluation: " ++ show err
               Right (v,_) -> (n,v):(evalAllConst sig xs)
      _ -> evalAllConst sig xs 


showAll :: Signature -> [Declaration] -> IO ()
showAll sig decl = let ls = map (showConst sig) (evalAllConst sig decl) in
                  sequence_ (map putStrLn ls)

showConst :: Signature -> (Name,Val) -> String
showConst sig (n,v) = let s = prettyVal sig v in
                          case s of
                            Left err -> "error"
                            Right (str,_) -> n ++ " evaluates to " ++ str

termCheckAll :: [Declaration] -> IO ()
termCheckAll dl = do _ <- mapM terminationCheckDecl dl
                     return  ()

doTypeCheck :: [Declaration] -> IO (Maybe Signature)
doTypeCheck decl = do let k = typeCheck decl
                      case k of
                        Left err -> do putStrLn $ "error during typechecking:\n" ++ show err
                                       return Nothing
                        Right (_,sig) -> do return $ Just sig

doScopeCheck :: [Declaration] -> [Declaration]
doScopeCheck decl = let k = scopeCheck decl 
                    in
                      case k of
                        Left err -> error $ "scope check error: " ++ err
                        Right (decl',_) -> decl'