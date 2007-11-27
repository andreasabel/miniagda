module Main where

import Tokens
import Lexer
import Parser

import qualified Concrete as C
import qualified Abstract as A
import Abstract (Name)
import ScopeChecker
import TypeChecker
import Value

import System

import System.IO (stdout, hSetBuffering, BufferMode(..))


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn $ "***** Mugda v1.0 *****"
  args <- getArgs
  file <- readFile (args !! 0)
  t <- return $ alexScanTokens file 
  ast <- return $ parse t
  putStrLn ("--- scope checking ---")
  ast2 <- return $ doScopeCheck ast
  putStrLn ("--- type checking ---")
  tc <- doTypeCheck ast2
  case tc of
    Nothing -> return ()
    Just sig -> do putStrLn "--- evaluating constants ---" 
                   showAll sig ast2
  

-- all constants
allConst :: Signature -> [A.Declaration] -> [(Name,A.Expr)]
allConst sig [] = []
allConst sig (decl:xs) =
    case decl of
      (A.ConstDecl True (A.TypeSig n t) e) -> 
          (n,e):(allConst sig xs)
      _ -> allConst sig xs 


showAll :: Signature -> [A.Declaration] -> IO ()
showAll sig decl = do ls <- mapM (showConst sig) (allConst sig decl) 
                      sequence_ (map putStrLn ls)

showConst :: Signature -> (Name,A.Expr) -> IO String
showConst sig (n,e) = do Right (v,_) <- doWhnf sig e 
                         return $ n ++ " evaluates to " ++ show v

doTypeCheck :: [A.Declaration] -> IO (Maybe Signature)
doTypeCheck decl = do k <- typeCheck decl
                      case k of
                        Left err -> do putStrLn $ "error during typechecking:\n" ++ show err
                                       return Nothing
                        Right (_,sig) -> do return $ Just sig

doScopeCheck :: [C.Declaration] -> [A.Declaration]
doScopeCheck decl = let k = scopeCheck decl 
                    in
                      case k of
                        Left err -> error $ "scope check error: " ++ err
                        Right (decl',_) -> decl'