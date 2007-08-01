module SizeChecker where

-- some basic checking of the usage of sizes in data and function declarations

import Abstract
import Debug.Trace



sizeCheck :: [Declaration] -> [Bool]
sizeCheck = map sizeCheckDeclaration

sizeCheckDeclaration :: Declaration -> Bool
sizeCheckDeclaration (Declaration tsl dl)  = all sizeCheckDefinition (zip tsl dl)

sizeCheckDefinition :: (TypeSig,Definition) -> Bool
sizeCheckDefinition (TypeSig n t,DataDef _ tel cl) = 
    case t of
      (Fun Size e2) -> -- is a sized type
           withoutSize e2 && withoutSizeTelescope tel 
                          && all (sizeCheckConstructor (length tel) n) cl
      _ -> -- not a sized type 
           withoutSize t && withoutSizeTelescope tel  
sizeCheckDefinition (TypeSig n t,FunDef _ cl) = sizeCheckFunType t
sizeCheckDefinition (TypeSig n t,ConstDef e) = sizeCheckFunType t



-------------
-- check data type constructor 
-- the size argument is expected at position i of all inductive arguments 
-- 

sizeCheckConstructor :: Int -> Name -> Constructor -> Bool
sizeCheckConstructor i n (TypeSig _ t) = 
    let iargs = reverse $ getInductiveArguments n i t in 
    case head iargs of
      Succ _ -> all checkVar (tail iargs) 
                || ( error $ "SizeCheck Error: Inductive Argument in " ++ n ++ "is not a size variable")          
          where checkVar (Var _) = True
                checkVar (Succ _) = False
      _ -> error $ "SizeCheck Error: Constructor " ++  n ++ " does not increase size "                       
      

-- the last element returned is the result of the constructor
getInductiveArguments :: Name -> Int -> Expr -> [Expr]
getInductiveArguments n i e = 
    case e of
      App (Def m) e2 -> if n == m then [e2 !! i] else concatMap (getInductiveArguments n i) e2
      Fun e1 e2 -> (getInductiveArguments n i e1) ++ (getInductiveArguments n i e2)
      Pi (TBind _ e1) e2 -> (getInductiveArguments n i e1) ++ (getInductiveArguments n i e2)
      _ -> []

withoutSize :: Expr -> Bool
withoutSize e = 
    case e of
      Size -> False
      App e1 el -> withoutSize e1 && all withoutSize el
      Lam n e2 -> withoutSize e2
      Fun e1 e2 -> withoutSize e1 && withoutSize e2
      Pi (TBind n e1) e2 -> withoutSize e1 && withoutSize e2
      _ -> True

withoutSizeTelescope :: Telescope -> Bool
withoutSizeTelescope  = all (\(TBind _ e) -> withoutSize e) 

---- check function type declaration
-- the declaration should look something like (i:Size -> (j:Size) -> X where i,j are used in X and X is without further size arguments
  
sizeCheckFunType :: Type -> Bool
sizeCheckFunType t = 
    case t of 
      (Pi (TBind n Size) e) -> (sizeVarIsUsed n e 
                                || (error $ "SizeCheck error " ++ n ++ "not used in function declaration) " ++ show t)) 
                               && sizeCheckFunType e
      _ -> withoutSize t || (error $ "SizeCheck error: Size expression used in " ++ show t)

-- test if is size var n used in e
sizeVarIsUsed :: Name -> Expr -> Bool
sizeVarIsUsed n e = let ft = getFunArgTypes e in
                    any (sizeArg n) ft
    where
 
      sizeArg :: Name -> Expr -> Bool
      sizeArg n e@(App e1 e2) = any (sizeStageVar n) e2  
      sizeArg _ _ = False
      sizeStageVar :: Name -> Expr -> Bool
      sizeStageVar n (Succ e) = sizeStageVar n e
      sizeStageVar n (Var x) = n == x
      sizeStageVar _ _ = False

-- get the arguments 
getFunArgTypes :: Type -> [Expr]
getFunArgTypes e = case e of
                  (Fun e1 e2) -> e1 : getFunArgTypes e2
                  (Pi (TBind n e1) e2) -> e1 : getFunArgTypes e2
                  _ -> []
