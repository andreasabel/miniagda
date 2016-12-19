{- delta size change termination

implement a simple language for doing SCT experiments

Syntax for input:

  f(x,y,z) = f(x+100,x-50,y) | g(y,x-1)
  g(a,b)   = f(b,1000)

-}

module Main where

import SCTSyntax 
import Lexer (alexScanTokens)
import SCTParser
import qualified Termination as T

import Control.Monad -- when
import Debug.Trace
import System

usage :: IO ()
usage = do
  mapM_ putStrLn 
   [ "Size-Change Termination"
   , "usage: SCT file [cutoff]"
   , "file :: String"
   , "cutoff :: Int > 0"
   ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  when (length args < 1) $ usage
  let fileName:rest = args  
  let ?cutoff = if (null rest) then 0 else read (head rest)
  mainFile fileName

mainFile :: (?cutoff :: Int) => String -> IO ()
mainFile fileName = do
  file <- readFile fileName
  let Defs defs = parse $ alexScanTokens file
  putStrLn $ show $ Defs defs 
  let names = map (defFun . lhs) defs
  -- let ?cutoff = (maxBound :: Int)
  let calls = concat $ map callsDef defs
  let beh = sizeChangeTermination names calls
  putStrLn $ show beh

callsDef :: (?cutoff :: Int) => Def -> [T.Call] 
callsDef (Def (LHS f xs) rhs) = map (\ (Call g as) -> T.Call f g $ callMat xs as) rhs

callMat :: (?cutoff :: Int) => [Param] -> [Arg] -> T.CallMatrix
callMat xs as = map (\ a -> map (\ (Param x) -> compArgPar a x) xs) as

compArgPar :: (?cutoff :: Int) => Arg -> Name -> T.Order
compArgPar a x = case a of
   Const _ -> T.Un
   Plus y i | x == y    -> T.decr (- i)
            | otherwise -> T.Un

sizeChangeTermination :: (?cutoff :: Int) => [Name] -> [T.Call] -> [(Name,Bool)]
sizeChangeTermination names cg0 =
   let cg1 = T.makeCG names cg0
       cg = T.complCGraph $ cg1
       beh = zip names $ map (all (T.checkIdem . snd)) $ T.diag cg 
   in trace ("call graph: " ++ show cg0) $
      trace ("normalized call graph: " ++ show cg1) $
      trace ("completed call graph: " ++ show cg) $
      trace ("recursion behaviours" ++ show beh) $
      beh  
