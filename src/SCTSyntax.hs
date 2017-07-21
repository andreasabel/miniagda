{-# LANGUAGE NoImplicitPrelude #-}
module SCTSyntax where

import Prelude hiding (showList)

-- grammar -----------------------------------------------------------

-- identifiers for parameters and functions
type Name = String

newtype Param = Param Name

-- expressions for arguments of recursive calls
data Arg = Const Int       -- 1
         | Plus  Name Int  -- x + 1, y - 2

-- a recursive call f(as) with as non-empty
data Call = Call { callFun :: Name, callArgs :: [Arg] } -- f(x+1,y-2)

-- a rhs is a non-empty list of calls
type RHS = [Call] -- f(x+1,y-2) | g(z,5)

-- a lhs f(xs) with xs non-empty
data LHS = LHS { defFun :: Name, defPars :: [Param] } -- f(x,y)

data Def = Def { lhs :: LHS, rhs :: RHS }

-- a possibly empty list of defs
data Defs = Defs { defs :: [Def] }

-- printing ----------------------------------------------------------

instance Show Param where
  show (Param x) = x

instance Show Arg where
  show (Const i) = show i
  show (Plus x i) | i == 0 = x
                  | i < 0  = x ++ show i
                  | i > 0  = x ++ "+" ++ show i

instance Show LHS where
  show (LHS f xs)  = f ++ roundParens (show xs)

instance Show Call where
  show (Call f as) = f ++ roundParens (show as)

instance Show Def where
  show (Def l r) = show l ++ " = " ++ showList " | " r

instance Show Defs where
  show (Defs ds) = foldr (\ d s -> show d ++ "\n" ++ s) "" ds

showList :: (Show a) => String -> [a] -> String
showList sep [] = ""
showList sep [a] = show a
showList sep (a:as) = show a ++ sep ++ showList sep as

noParens ('[':s) = take (length s - 1) s
roundParens ('[':s) = '(' : take (length s - 1) s ++ ")"
