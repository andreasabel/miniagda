{-# LANGUAGE NamedFieldPuns #-}
-- concrete syntax
module Concrete where

import Prelude hiding (null)

import Util
import Abstract
  (Co, Sized, PiSigma(..), Dec, Override(..), Measure(..), Bound(..), HasPred(..), LtLe(..), polarity)
import Polarity

-- | Concrete names.
data Name = Name { theName :: String }
  deriving (Eq, Ord)

instance Show Name where
  show (Name n) = n

-- | Possibly qualified names.
data QName
  = Qual  { qual :: Name, name :: Name }  -- ^ @X.x@ e.g. qualified constructor.
  | QName { name :: Name }                -- ^ @x@.
  deriving (Eq, Ord)

unqual :: QName -> Name
unqual (QName n) = n

instance Show QName where
  show (Qual m n) = show m ++ "." ++ show n
  show (QName n)  = show n

set0 :: Expr
set0 = Set Zero

ident :: Name -> Expr
ident n = Ident (QName n)

-- | Concrete expressions syntax.
data Expr
  = Set Expr                        -- ^ Universe @Set e@; @Set@ for @Set 0@.
  | CoSet Expr
  | Size                            -- ^ @Size@ type of sizes.
  | Succ Expr                       -- ^ @$e@.
  | Zero                            -- ^ @0@.
  | Infty                           -- ^ @#@.
  | Max                             -- ^ @max@.
  | Plus Expr Expr                  -- ^ @e + e'@.
  | RApp Expr Expr                  -- ^ @e |> f@.
  | App Expr [Expr]                 -- ^ @f e1 ... en@ or @f <| e@.
  | Lam Name Expr                   -- ^ @\ x -> e@.
  | Case Expr (Maybe Type) [Clause] -- ^ @case e : A { cls }@.
  | LLet LetDef Expr                -- ^ @let x = e in e'@ local let.
  | Unfold Unfolds Expr             -- ^ @unfold f1, ..., fn in e@.
  | Quant PiSigma Telescope Expr    -- ^ @(x : A) -> B@, @[x : A] -> B@, @(x : A) & B@.
  | Pair Expr Expr                  -- ^ @e , e'@.
  | Record [([Name],Expr)]          -- ^ @record { x = e, x' y = e' }@.
  | Proj Name                       -- ^ @.x@.
  | Ident QName                     -- ^ @x@ or @D.c@.
  | Unknown                         -- ^ @_@.
  | Sing Expr Expr                  -- ^ @<e : A>@ singleton type.
--  | EBind TBind Expr                -- ^ @[x : A] B@
  deriving (Eq)

data LetDef = LetDef
  { letDefDec     :: Dec
  , letDefName    :: Name
  , letDefTel     :: Telescope
  , letDefType    :: Maybe Type
  , letDefUnfolds :: Unfolds
      -- ^ Definitions that may be unfolded to type check the body.
      --   Must be empty in local lets.
  , letDefExpr    :: Expr
  } deriving (Eq, Show)

instance Show Expr where
    show = prettyExpr

instance HasPred Expr where
  predecessor (Succ e) = Just e
  predecessor _ = Nothing

data Declaration
  = DataDecl Name Sized Co Telescope Type [Constructor]
      [Name] -- list of field names
  | RecordDecl Name Telescope Type Constructor
      [Name] -- list of field names
  | FunDecl Co TypeSig Unfolds [Clause]
  | LetDecl Bool LetDef -- True = if eval
  | PatternDecl Name [Name] Pattern
  | MutualDecl [Declaration]
  | OverrideDecl Override [Declaration] -- fail etc.
    deriving (Eq,Show)

-- | Definitions that can be unfolded.
type Unfolds = [Name]

data TypeSig = TypeSig Name Type
             deriving (Eq)

instance Show TypeSig where
  show (TypeSig n t) = show n ++ " : " ++ show t

type Type = Expr

data Constructor = Constructor
  { conName :: Name
  , conTel  :: Telescope
  , conType :: Maybe Type -- can be omitted *but* for families
  } deriving (Eq)

instance Show Constructor where
  show (Constructor n tel (Just t)) = show n ++ " " ++ show tel ++ " : " ++ show t
  show (Constructor n tel  Nothing) = show n ++ " " ++ show tel

type TBind = TBinding Type
type LBind = TBinding (Maybe Type)  -- possibly domain-free

data TBinding a = TBind
  { boundDec   :: Dec
  , boundNames :: [Name] -- [] if no name is given, then its a single bind
  , boundType  :: a
  }
  | TBounded  -- bounded quantification
  { boundDec   :: Dec
  , boundName  :: Name -- [] if no name is given, then its a single bind
  , ltle       :: LtLe
  , upperBound :: Expr
--  , boundMType :: Maybe Type -- type is inferred from upperBound
  }
  | TMeasure (Measure Expr)
  | TBound (Bound Expr)
--  | TSized { boundName :: Name } -- the size parameter of a sized record
    deriving (Eq,Show)

type Telescope = [TBind]

data DefClause = DefClause
   Name         -- function identifier
   [Elim]
   (Maybe Expr) -- Nothing for absurd pattern clause
 deriving (Eq,Show)

data Elim
  = EApp Pattern          -- application to a pattern
  | EProj Name [Pattern]  -- projection with arguments
    deriving (Eq,Show)

data Clause = Clause
                (Maybe Name) -- Just funId | Nothing for case clauses
                [Pattern]
                (Maybe Expr) -- Nothing for absurd pattern clause
            deriving (Eq,Show)

data Pattern
  = ConP Bool QName [Pattern] -- ^ @(c ps)@ if @False; @(.c ps)@ if @True@.
  | PairP Pattern Pattern     -- ^ @(p, p')@
  | SuccP Pattern             -- ^ @($ p)@
  | DotP Expr                 -- ^ @.e@
  | IdentP QName              -- ^ @x@ or @c@ or @D.c@.
  | SizeP Expr Name           -- ^ @(x > y)@ or @y < #@ or ...
  | AbsurdP                   -- ^ @()@
    deriving (Eq,Show)

type Case = (Pattern,Expr)

-- | Used in Parser.
patApp :: Pattern -> [Pattern] -> Pattern
patApp (IdentP c)         ps' = ConP False  c ps'
patApp (ConP dotted c ps) ps' = ConP dotted c (ps ++ ps')

-- * Pretty printing.

prettyLBind :: LBind -> String
-- prettyLBind (TSized x)                   = prettyTBind False (TSized x)
prettyLBind (TMeasure mu)                = prettyTBind False (TMeasure mu)
prettyLBind (TBound (Bound ltle mu mu')) = prettyTBind False (TBound (Bound ltle mu mu'))
prettyLBind (TBounded dec x ltle e)      = prettyTBind False (TBounded dec x ltle e)
prettyLBind (TBind dec xs (Just t))      = prettyTBind False (TBind dec xs t)
prettyLBind (TBind dec xs Nothing) =
  if erased dec then addPol False $ brackets binding
   else addPol True binding
  where binding = Util.showList " " show xs
        pol = polarity dec
        addPol b x = if pol==defaultPol
                      then x
                      else show pol ++ (if b then " " else "") ++ x


prettyTBind :: Bool -> TBind -> String
-- prettyTBind inPi (TSized x) = parens ("sized " ++ x)
prettyTBind inPi (TMeasure mu) = "|" ++
  (Util.showList ","  prettyExpr (measure mu)) ++ "|"
prettyTBind inPi (TBound (Bound ltle mu mu')) = "|" ++
  (Util.showList ","  prettyExpr (measure mu))  ++ "| " ++ show ltle ++ " |" ++
  (Util.showList ","  prettyExpr (measure mu')) ++ "|"
prettyTBind inPi (TBind dec xs t) =
  if erased dec then addPol False $ brackets binding
   else if (null xs) then addPol True s
   else addPol (not inPi) $ (if inPi then parens else id) binding
  where s = prettyExpr t
        binding = if null xs then s else
          foldr (\ x s -> show x ++ " " ++ s) (": " ++ s) xs
        pol = polarity dec
        addPol b x = if pol==defaultPol
                      then x
                      else show pol ++ (if b then " " else "") ++ x
prettyTBind inPi (TBounded dec x ltle e) =
  if erased dec then addPol False $ brackets binding
   else addPol (not inPi) $ (if inPi then parens else id) binding
  where binding = show x ++ " < " ++ prettyExpr e
        pol = polarity dec
        addPol b x = if pol==defaultPol
                      then x
                      else show pol ++ (if b then " " else "") ++ x
{-
prettyTBind :: Bool -> TBind -> String
prettyTBind inPi (TBind dec x t) =
  if erased dec then addPol False $ brackets binding
   else if x=="" then addPol True s
   else addPol (not inPi) $ (if inPi then parens else id) binding
  where s = prettyExpr t
        binding = if x == "" then s else x ++ " : " ++ s
        pol = polarity dec
        addPol b x = if pol==Mixed then x
                      else show pol ++ (if b then " " else "") ++ x
-}
prettyLetBody :: String -> Expr -> String
prettyLetBody s e = parens $ s ++ " in " ++ prettyExpr e

prettyLetAssign :: String -> Expr -> String
prettyLetAssign s e = "let " ++ s ++ " = " ++ prettyExpr e

prettyLetDef :: LetDef -> String
prettyLetDef (LetDef dec n [] mt unfolds e) = prettyLetAssign (prettyLBind tb ++ prettyUnfolds unfolds) e
  where tb = TBind dec [n] mt
prettyLetDef (LetDef dec n tel mt unfolds e) = prettyLetAssign s e
  where s = prettyDecId dec n ++ " " ++ prettyTel False tel ++ prettyMaybeType mt ++ prettyUnfolds unfolds

prettyUnfolds :: Unfolds -> String
prettyUnfolds [] = ""
prettyUnfolds ns = unwords [ " unfold", intercalate ", " (map show ns) ]

prettyDecId :: Dec -> Name -> String
prettyDecId dec x
  | erased dec = brackets $ show x
  | otherwise  =
     let pol = polarity dec
     in  if pol == defaultPol then show x else show pol ++ show x

prettyTel :: Bool -> Telescope -> String
prettyTel inPi = Util.showList " " (prettyTBind inPi)

prettyMaybeType :: Maybe Expr -> String
prettyMaybeType = maybe "" $ \ t -> " : " ++ prettyExpr t

prettyExpr :: Expr -> String
prettyExpr e =
    case e of
      -- Type e          -> "Type " ++ prettyExpr e
      CoSet e         -> "CoSet " ++ prettyExpr e
      Set e         -> "CoSet " ++ prettyExpr e
      -- Set             -> "Set"
      Size            -> "Size"
      Max             -> "max"
      Succ e          -> "$ " ++ prettyExpr e -- ++ ")"
      Zero            -> "0"
      Infty           -> "#"
      Plus e1 e2      -> "(" ++ prettyExpr e1 ++ " + " ++  prettyExpr e2 ++ ")"
      Pair e1 e2      -> "(" ++ prettyExpr e1 ++ " , " ++  prettyExpr e2 ++ ")"
      App e1 el       -> "(" ++ prettyExprs (e1:el) ++ ")"
      Lam x e1        -> "(\\" ++ show x ++ " -> " ++ prettyExpr e1 ++ ")"
      Case e Nothing cs -> "case " ++ prettyExpr e ++ " { " ++ Util.showList "; " prettyCase cs ++ " } "
      Case e (Just t) cs -> "case " ++ prettyExpr e ++ " : " ++ prettyExpr t ++ " { " ++ Util.showList "; " prettyCase cs ++ " } "
      LLet letdef e -> prettyLetBody (prettyLetDef letdef) e
      Unfold unfolds e -> unwords
        [ "unfold", intercalate ", " (map show unfolds), "in", prettyExpr e ]
{-
      LLet tb e1 e2 -> "(let " ++ prettyLBind tb ++ " = " ++ prettyExpr e1 ++ " in " ++ prettyExpr e2 ++ ")"
-}
      Record rs       -> "record {" ++ Util.showList "; " prettyRecordLine rs ++ "}"
      Proj n          -> "." ++ show n
      Ident n         -> show n
      Unknown         -> "_"
      Sing e t        -> "<" ++ prettyExpr e ++ " : " ++ prettyExpr t ++ ">"
--      Quant pisig tb t2 -> parens $ prettyTBind True tb
      Quant pisig tel t2 -> parens $ prettyTel True tel
                                  ++ " " ++ show pisig ++ " " ++ prettyExpr t2

prettyRecordLine :: ([Name], Expr) -> String
prettyRecordLine (xs, e) = Util.showList " " show xs ++ " = " ++ prettyExpr e

prettyCase :: Clause -> String
prettyCase (Clause Nothing [p] Nothing)  = prettyPattern p
prettyCase (Clause Nothing [p] (Just e)) = prettyPattern p ++ " -> " ++ prettyExpr e

prettyPattern :: Pattern -> String
prettyPattern (ConP dotted c ps) = parens $ foldl (\ acc p -> acc ++ " " ++ prettyPattern p) (if dotted then "." ++ show c else show c) ps
prettyPattern (PairP p1 p2) = parens $ prettyPattern p1 ++ ", " ++
                                prettyPattern p2
prettyPattern (SuccP p)   = parens $ "$ " ++ prettyPattern p
prettyPattern (DotP e)    = "." ++ prettyExpr e
prettyPattern (IdentP x)  = show x
prettyPattern (SizeP e y) = parens $ prettyExpr e ++ " > " ++ show y
prettyPattern (AbsurdP)   = parens ""

prettyExprs :: [Expr] -> String
prettyExprs = Util.showList " " prettyExpr

prettyDecl :: Declaration -> String
prettyDecl (PatternDecl n ns p) = "pattern " ++ (Util.showList " " show (n:ns)) ++ " = " ++ prettyPattern p

teleToType :: Telescope -> Type -> Type
teleToType [] t = t
teleToType (tb:tel) t2 = Quant Pi [tb] (teleToType tel t2)
--teleToType (PosTB dec n t:tel) t2 = Pi dec n t (teleToType tel t2)

typeToTele :: Type -> (Telescope, Type)
typeToTele (Quant Pi tel0 c) =
  let (tel, a) = typeToTele c in (tel0 ++ tel, a)
typeToTele a = ([],a)

{-
teleToType :: Telescope -> Type -> Type
teleToType [] t = t
teleToType (tb:tel) t2 = Quant Pi tb (teleToType tel t2)
--teleToType (PosTB dec n t:tel) t2 = Pi dec n t (teleToType tel t2)

typeToTele :: Type -> (Telescope, Type)
typeToTele = typeToTele' (-1)

typeToTele' :: Int -> Type -> (Telescope, Type)
typeToTele' k (Quant A.Pi tb c) | k /= 0 =
  let (tel, a) = typeToTele' (k-1) c in (tb:tel, a)
typeToTele' _ a = ([],a)
-}

teleNames :: Telescope -> [Name]
teleNames tel = concat $ map tbindNames tel

tbindNames :: TBind -> [Name]
tbindNames TBind{ boundNames }   = boundNames
tbindNames TBounded{ boundName } = [boundName]
-- tbindNames TSized{ boundName }   = [boundName]
tbindNames tb = error $ "tbindNames (" ++ show tb ++ ")"
