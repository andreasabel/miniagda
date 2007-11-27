-- completness of size patterns

module Completness where

import Abstract

completeFun :: [Clause] -> Bool
completeFun cl = all cf cl  
    where cf (Clause pl _) = all cp pl

-- complete pattern
cp :: Pattern -> Bool
cp (SuccP (VarP i)) = False
cp (SuccP x) = cp x
cp (ConP _ _ pl) = all cp pl
cp _ = True
