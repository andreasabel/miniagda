-- 2010-06-20
-- this module has been RETIRED for some time already

-- completness of size patterns

module Completness where

import Data.List -- nub

import Abstract

-- fail if somewhere there is a size pattern ($ i) which is accessible
completeFun :: [Clause] -> Bool
completeFun cl = all cf cl  
    where cf (Clause pl _) = all cp pl

-- complete pattern
cp :: Pattern -> Bool
cp (SuccP (VarP i)) = False
cp (SuccP x) = cp x
cp (ConP _ _ pl) = all cp pl
cp _ = True

-- coFunSuccPat i cls
-- check whether in each clause, pattern i is size successor
coFunSuccPat :: Int -> [Clause] -> Bool
coFunSuccPat i cls = all (\ (Clause pats e) -> isSuccPat (pats !! i)) cls

isSuccPat :: Pattern -> Bool
isSuccPat (SuccP (VarP i)) = True
isSuccPat (DotP (Succ (Var i))) = True
isSuccPat _ = False

-- find all size matches which are not a variable

getSizeMatches :: [Clause] -> [Int]
getSizeMatches cls = nub $ concat $ map clsSizeMatches cls

clsSizeMatches :: Clause -> [Int]
clsSizeMatches (Clause pats _) = 
  map snd $ filter (\ (pat,_) -> isSuccPat pat) $ zip pats [0..]