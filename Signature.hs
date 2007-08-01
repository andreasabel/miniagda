module Signature where

import Abstract

type Signature = [(Name,SigDef)] 

data SigDef = FunSig Co Type Int [Clause] --type , co , arity , clauses
            | ConstSig Type Int Expr -- type , arity 
            | ConSig Type Int -- type , arity  
            | DataSig Co Telescope Type Int -- parameters, co , type , arity 
              deriving (Show)

emptySig = []

lookupSig:: Signature -> Name -> SigDef
lookupSig sig n = case (lookup n sig) of
                       Nothing -> error $ "Error not in signature: "  ++ n ++ " " ++ show sig
                       Just k -> k

addSig :: Signature -> Name -> SigDef -> Signature
addSig sig n def = sig ++ [(n,def)]