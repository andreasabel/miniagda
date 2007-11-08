-- completness of size patterns

module Completness where

import Abstract

completeFun :: [Clause] -> Bool
completeFun cl = all cf cl  
    where cf (Clause pl _) = all cp pl


-- the adm size argument does not have to be complete
completeCoFun :: [Clause] -> Int -> Bool
completeCoFun cl adm = all cf cl  
    where cf (Clause pl _) = let pl' = take adm pl ++ drop (adm+1) pl 
                             in all cp pl'

-- complete pattern
cp :: Pattern -> Bool
cp (SuccP (VarP i)) = False
cp (SuccP x) = cp x
cp (ConP _ _ pl) = all cp pl
cp _ = True
