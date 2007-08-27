module Termination where

import Abstract
import Data.List as List
import qualified Data.Set as Set
import Debug.Trace


import System


data Order = Lt
           | Le
           | Un
           deriving (Eq,Show)

instance Ord Order where
    _ <= Lt = True
    Un <= _ = True
    Le <= Le = True
    _ <= _ = False

comp :: Order -> Order -> Order
comp Lt Un = Un
comp Lt  _ = Lt
comp Le o = o
comp Un  _ = Un

supremum :: [Order] -> Order
supremum = foldr max Un

infimum :: [Order] -> Order
infimum = foldr min Lt

type Vector a = [a]
type Matrix a = [Vector a]


---


compareArgs :: [Pattern] -> [Expr] -> Matrix Order
compareArgs [] [] = [[]]
compareArgs pl el = map (\ e -> (map (compareExpr e) pl)) el
                    
compareExpr :: Expr -> Pattern -> Order
compareExpr e p = supremum $ (compareExpr' e p) : (map cmp (subPatterns p))
    where cmp p' = comp Lt (compareExpr' e p') -- if e Le p' , then e Lt p

subPatterns :: Pattern -> [Pattern]
subPatterns p = case p of
                  (ConP c pl) -> pl ++ (concatMap subPatterns pl)
                  (SuccP p2) -> p2 : subPatterns p2
                  _ -> []

compareExpr' :: Expr -> Pattern -> Order
compareExpr' e p =  
    case (e,p) of 
      ((Var i),p) -> compareVar i p 
      ((App (Con n) args),(ConP n2 pl)) -> if ( n == n2 ) then
                                               if null args then 
                                                   Le
                                               else
                                                   infimum (zipWith compareExpr' args pl)
                                           else
                                               Un
      (Succ e2,SuccP p2) -> compareExpr' e2 p2
      -- needed for ordinal addition f <= f n 
      (App (Var f) args,VarP g) -> if (f == g) then 
                                 Le
                             else
                                 Un
      _ -> Un

compareVar :: Name -> Pattern -> Order
compareVar n p = 
    case p of
      (VarP n2) -> if n == n2 then Le else Un
      (ConP c pl) -> comp Lt (supremum (map (compareVar n) pl))
      (SuccP p2) -> comp Lt (compareVar n p2)
      (DotP e) -> Un
      _ -> error $ "comparevar " ++ show n ++ "\n" ++ show p
