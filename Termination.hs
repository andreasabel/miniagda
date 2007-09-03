module Termination where

import Abstract
import Data.List as List
import qualified Data.Set as Set
import Debug.Trace


import System


data Order = Lt
           | Le
           | Un
           | Mat (Matrix Order)
           deriving (Show,Eq,Ord)

ordRing :: SemiRing Order
ordRing = SemiRing { add = maxO , mul = comp , one = Le , zero = Un }

comp :: Order -> Order -> Order
comp Lt Un = Un
comp Lt _ = Lt
comp Le o = o
comp Un _ = Un
comp (Mat m1) (Mat m2) = let m1m2 = Mat $mmul ordRing m1 m2
                             cm1m2 = comp (collabse m1) (collabse m2)
                         in
                           if (compatible m1 m2) then 
                               m1m2
                           else
                               cm1m2
comp (Mat m1) x = comp (collabse m1) x

collabse :: Matrix Order -> Order
collabse m = foldl comp Le (diag m)

maxO :: Order -> Order -> Order
maxO o1 o2 = case (o1,o2) of 
               (_,Lt) -> Lt
               (Lt,_) -> Lt
               (Un,_) -> o2
               (_,Un) -> o1
               (Mat m,_) -> maxO (collabse m) o2
               (_,Mat m) ->  maxO o1 (collabse m)
               (Le,Le) -> Le

supremum :: [Order] -> Order
supremum = foldr maxO Un

--infimum :: [Order] -> Order
--infimum = foldr min Lt

type Vector a = [a]
type Matrix a = [Vector a]

compatible :: Matrix a -> Matrix a -> Bool 
compatible m1 m2 = (length m2) == (length m1) -- assumed square 

--- 
-- matrix stuff

data SemiRing a = SemiRing { add :: (a -> a -> a) , mul :: (a -> a -> a) , one :: a , zero :: a } 

ssum :: SemiRing a -> Vector a -> a
ssum sem v = foldl (add sem) (zero sem) v 

vadd :: SemiRing a -> Vector a -> Vector a -> Vector a
vadd sem v1 v2 = [ (add sem) x y | (x,y) <- zip v1 v2]

scalarProdukt :: SemiRing a -> Vector a -> Vector a -> a
scalarProdukt sem xs ys = ssum sem [(mul sem) x y  | (x,y) <- zip xs ys]

madd :: SemiRing a -> Matrix a -> Matrix a -> Matrix a
madd sem v1 v2 = [ vadd sem x y | (x,y) <- zip v1 v2]

transp :: Matrix a -> Matrix a 
transp y = [[ z!!j | z<-y] | j<-[0..s]]
    where
    s = length (head y)-1

mmul :: SemiRing a -> Matrix a -> Matrix a -> Matrix a
mmul sem m1 m2 = [[scalarProdukt sem r c | c <- transp m2] | r<-m1 ]

diag :: Matrix a -> Vector a
diag m = [ (m !! j) !! j | j <- [ 0..s] ] 
   where
     s = length (head m) - 1

elems :: Matrix a -> Vector a
elems m = concat m




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
                                               if length args <= 0 then
                                                   foldl comp Le $ 
                                                         map (compareExpr' (head args)) pl
                                               else
                                                   let m' = map (\e'' -> (map (compareExpr' e'') pl)) args 
                                                   in
                                                     Mat m'
                                           else
                                               Un
      (Con n,ConP n2 []) -> if n == n2 then Le else Un    
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
      WildP -> Un
      _ -> error $ "comparevar " ++ show n ++ "\n" ++ show p
