module Signature where

import Abstract

type Signature = [(Name,SigDef)] 

data SigDef = FunSig Co Type Int [Clause] --type , co , arity , clauses
            | ConstSig Type Expr -- type , expr 
            | ConSig Type -- type   
            | DataSig Co Type -- parameters, co , type  
              deriving (Show)

emptySig = []

lookupSigMaybe :: Name -> Signature -> Maybe SigDef
lookupSigMaybe = lookup

lookupSig :: Name -> Signature -> SigDef
lookupSig n sig = case (lookup n sig) of
                       Nothing -> error $ "Error not in signature: "  ++ n ++ " " ++ show sig
                       Just k -> k

addSig :: Signature -> Name -> SigDef -> Signature
addSig sig n def = sig ++ [(n,def)]