module Signature where

import Abstract

type Signature = [(Name,SigDef)] 

data SigDef = FunSig Type [Clause]
         | ConstSig Type Expr
         | ConSig Type
         | DataSig Telescope Type
           deriving (Show)

emptySig = []

lookupSig:: Signature -> Name -> SigDef
lookupSig sig n = case (lookup n sig) of
                       Nothing -> error $ "Error not in signature: "  ++ n ++ " " ++ show sig
                       Just k -> k

addSig :: Signature -> Name -> SigDef -> Signature
addSig sig n def = sig ++ [(n,def)]