module Signature where

import Abstract
import Value

type Signature = [(Name,SigDef)] 

data SigDef = FunSig Co TVal Int [Clause] --type , co , arity , clauses
            | ConstSig TVal Expr -- type , expr 
            | ConSig TVal -- type   
            | DataSig Co TVal -- parameters, co , type  
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