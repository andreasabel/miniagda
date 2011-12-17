{ 
module Parser where

import qualified Lexer as T
import qualified Concrete as C

import Abstract (Decoration(..),Dec,defaultDec,Override(..))
import Polarity (Pol(..))
import qualified Abstract as A
import qualified Polarity as A
import Concrete (Name)
}

%name parse
%tokentype { T.Token }
%error { parseError }

%token

id      { T.Id $$ _ }
number  { T.Number $$ _ }
data    { T.Data _ }
codata  { T.CoData _ }
record  { T.Record _ }
sized   { T.Sized _ }
fields  { T.Fields _ }
mutual  { T.Mutual _ }
fun     { T.Fun _ }
cofun   { T.CoFun _ } 
case    { T.Case _ }
def     { T.Def _ }
let     { T.Let _ }
in      { T.In _ }
eval    { T.Eval _ }
fail    { T.Fail _ }
check   { T.Check _ }
trustme { T.TrustMe _ }
impredicative { T.Impredicative _ }
type    { T.Type _ }
set     { T.Set _ }
coset   { T.CoSet _ }
size    { T.Size _ }
infty   { T.Infty _ }
succ    { T.Succ _ }
max     { T.Max _ }
'<'     { T.AngleOpen _ }
'>'     { T.AngleClose _ }
'{'     { T.BrOpen _ }
'}'     { T.BrClose _ }
'['     { T.BracketOpen _ }
']'     { T.BracketClose _ }
'('     { T.PrOpen _ }
')'     { T.PrClose _ }
'|'     { T.Bar _ }
','     { T.Comma _ }
';'     { T.Sem _ }
':'     { T.Col _ }
'.'     { T.Dot _ }
'->'    { T.Arrow _ }
'<='    { T.Leq _ }
'='     { T.Eq _ }
'++'    { T.PlusPlus _ }
'+'     { T.Plus _ }
'-'     { T.Minus _ }
'/'     { T.Slash _ } -- UNUSED
'*'     { T.Times _ } -- UNUSED
'^'     { T.Hat _ } 
'&'     { T.Amp _ } 
'\\'    { T.Lam _ }
'_'     { T.Underscore _ }

%%

TopLevel :: { [C.Declaration] }
TopLevel : Declarations { reverse $1}


Declarations :: { [C.Declaration] }
Declarations : {- empty -} { [] }
             | Declarations Declaration { $2 : $1 }

Declaration :: { C.Declaration }
Declaration : Data                      { $1 }
           | CoData                     { $1 }
           | SizedData                  { $1 }
           | SizedCoData                { $1 }
           | RecordDecl                 { $1 }
           | Fun                        { $1 }
           | CoFun                      { $1 }
           | Mutual                     { $1 }
           | Let                        { $1 }
           | impredicative Declaration          { C.OverrideDecl Impredicative [$2] }
           | impredicative '{' Declarations '}' { C.OverrideDecl Impredicative $3 }
           | fail Declaration             { C.OverrideDecl Fail [$2] }
           | fail '{' Declarations '}'    { C.OverrideDecl Fail $3 }
           | check Declaration            { C.OverrideDecl Check [$2] }
           | check '{' Declarations '}'   { C.OverrideDecl Check $3 }
           | trustme Declaration          { C.OverrideDecl TrustMe [$2] }
           | trustme '{' Declarations '}' { C.OverrideDecl TrustMe $3 }

Data :: { C.Declaration }
Data : data Id DataTelescope ':' Expr '{' Constructors '}' OptFields
   { C.DataDecl $2 A.NotSized A.Ind $3 $5 (reverse $7) $9 }

SizedData :: { C.Declaration }
SizedData : sized data Id DataTelescope ':' Expr '{' Constructors '}' OptFields
   { C.DataDecl $3 A.Sized A.Ind $4 $6 (reverse $8) $10 }

CoData :: { C.Declaration }
CoData : codata Id DataTelescope ':' Expr '{' Constructors '}' OptFields 
       { C.DataDecl $2 A.NotSized A.CoInd $3 $5 (reverse $7) $9 }

SizedCoData :: { C.Declaration }
SizedCoData : sized codata Id DataTelescope ':' Expr '{' Constructors '}' OptFields
       { C.DataDecl $3 A.Sized A.CoInd $4 $6 (reverse $8) $10 }

RecordDecl :: { C.Declaration }
RecordDecl : record Id DataTelescope ':' Expr '{' Constructor '}'  OptFields
   { C.RecordDecl $2 $3 $5 $7 $9 }

Fun :: { C.Declaration }
Fun : fun TypeSig '{' Clauses '}' { C.FunDecl A.Ind $2 $4 }

CoFun :: { C.Declaration }
CoFun : cofun TypeSig '{' Clauses '}' { C.FunDecl A.CoInd $2 $4  }



Mutual :: { C.Declaration }
Mutual : mutual '{' Declarations '}' { C.MutualDecl (reverse $3) }
     

 
Let :: { C.Declaration }
Let : let TypeSig '=' ExprT { C.LetDecl False $2 $4 } 
      | eval let TypeSig '=' ExprT { C.LetDecl True $3 $5 } 

OptFields :: { [Name] }
OptFields : {- empty -}  { [] }
          | fields Ids   { $2 }
-----

Id :: { Name }
Id : id { $1 }
-- no longer  number { $1 }

SpcIds :: { [Name] } -- non-empty list
SpcIds : Id     { [$1] }
       | Id SpcIds { $1 : $2 }

Ids :: { [Name] } -- non-empty list
Ids : Id              { [$1] }
    | Id ',' Ids { $1 : $3 }

Pol :: { Pol }
Pol : '++'         { SPos  }
    | '+'          { Pos   }
    | '-'          { Neg   }
    | '.'          { Const } -- use bracket [..]
    | '^'          { Param }
    | '*'          { Rec   } -- recursive
--    | {- empty -}  { Mixed }

Measure :: { A.Measure C.Expr }
Measure : '|' Meas { A.Measure $2 }

Meas :: { [C.Expr] }
Meas : Expr '|'      { [$1] }
     | Expr ',' Meas { $1 : $3 }

Bound :: { A.Bound C.Expr }
Bound : Measure '<' Measure { A.Bound A.Lt $1 $3 }
      | Measure '<=' Measure { A.Bound A.Le $1 $3 } {- (A.succMeasure C.Succ $3) } -}


TBind :: { C.TBind }
TBind :  '(' Ids ':' Expr ')' { C.TBind (Dec Default) {- A.defaultDec -} $2 $4 } -- ordinary binding
      |  '[' Ids ':' Expr ']' { C.TBind A.irrelevantDec $2 $4 }  -- erased binding
      |  Pol '(' Ids ':' Expr ')' { C.TBind (Dec $1) $3 $5 } -- ordinary binding
--      |  Pol '[' Ids ':' Expr ']' { C.TBind (Dec True $1) $3 $5 }  -- erased binding
      | '(' Id '<'  Expr ')'  { C.TBounded A.defaultDec    $2 A.Lt $4 }
      | '[' Id '<'  Expr ']'  { C.TBounded A.irrelevantDec $2 A.Lt $4 }
      | '(' Id '<=' Expr ')'  { C.TBounded A.defaultDec    $2 A.Le $4 }
      | '[' Id '<=' Expr ']'  { C.TBounded A.irrelevantDec $2 A.Le $4 }

-- let binding
LBind :: { C.TBind }
LBind :  Id ':' Expr         { C.TBind A.defaultDec [$1] $3 } -- ordinary binding
      |  '(' Id ':' Expr ')' { C.TBind A.defaultDec [$2] $4 } -- ordinary binding
      |  '[' Id ':' Expr ']' { C.TBind A.irrelevantDec [$2] $4 }  -- erased binding
      |  Pol '(' Id ':' Expr ')' { C.TBind (Dec $1) [$3] $5 } -- ordinary binding
--      |  Pol '[' Id ':' Expr ']' { C.TBind (Dec True $1) [$3] $5 }  -- erased binding
Domain :: { C.TBind }
Domain : Expr1             { C.TBind (Dec Default) {- A.defaultDec -} [] $1 }
       | '[' Expr ']'      { C.TBind A.irrelevantDec [] $2 }
       | Pol Expr1         { C.TBind (Dec $1) [] $2 }
--       | Pol '[' Expr ']'  { C.TBind (Dec True  $1) [] $3 }
       | TBind             { $1 }
       | Measure           { C.TMeasure $1 }
       | Bound             { C.TBound $1 }

{-
TBind :: { C.TBind }
TBind :  '(' Ids ':' Type ')' { C.TBind (Dec Default) {- A.defaultDec -} $2 $4 } -- ordinary binding
      |  '[' Ids ':' Type ']' { C.TBind A.irrelevantDec $2 $4 }  -- erased binding
      |  Pol '(' Ids ':' Type ')' { C.TBind (Dec $1) $3 $5 } -- ordinary binding
--      |  Pol '[' Ids ':' Expr ']' { C.TBind (Dec True $1) $3 $5 }  -- erased binding
      | '(' Id '<'  Expr ')'  { C.TBounded A.defaultDec    $2 A.Lt $4 }
      | '[' Id '<'  Expr ']'  { C.TBounded A.irrelevantDec $2 A.Lt $4 }
      | '(' Id '<=' Expr ')'  { C.TBounded A.defaultDec    $2 A.Le $4 }
      | '[' Id '<=' Expr ']'  { C.TBounded A.irrelevantDec $2 A.Le $4 }

-- let binding
LBind :: { C.TBind }
LBind :  Id ':' Type         { C.TBind A.defaultDec [$1] $3 } -- ordinary binding
      |  '(' Id ':' Type ')' { C.TBind A.defaultDec [$2] $4 } -- ordinary binding
      |  '[' Id ':' Type ']' { C.TBind A.irrelevantDec [$2] $4 }  -- erased binding
      |  Pol '(' Id ':' Type ')' { C.TBind (Dec $1) [$3] $5 } -- ordinary binding
--      |  Pol '[' Id ':' Type ']' { C.TBind (Dec True $1) [$3] $5 }  -- erased binding

Domain :: { C.TBind }
Domain : Type1             { C.TBind (Dec Default) {- A.defaultDec -} [] $1 }
       | '[' Type ']'      { C.TBind A.irrelevantDec [] $2 }
       | Pol Type1         { C.TBind (Dec $1) [] $2 }
--       | Pol '[' Type ']'  { C.TBind (Dec True  $1) [] $3 }
       | TBind             { $1 }
       | Measure           { C.TMeasure $1 }
       | Bound             { C.TBound $1 }
-}

-- expressions which can be tuples e , e'
ExprT :: { C.Expr}
ExprT : Expr               { $1 }
      | Expr ',' ExprT     { C.Pair $1 $3 }
--      | '(' ExprT ')'      { $2 }

-- general form of expression
Expr :: { C.Expr }
Expr : Domain '->' Expr                 { C.Quant A.Pi $1 $3 } 
--     | Domain '&' Expr                  { C.Quant A.Sigma $1 $3 } 
     | '\\' SpcIds '->' ExprT           { foldr C.Lam $4 $2 }
     | let LBind '=' ExprT in ExprT     { C.LLet $2 $4 $6 }
     | case ExprT '{' Cases '}'          { C.Case $2 $4 }  
     | Expr1                            { $1 }
     | Expr1 '+' Expr                   { C.Plus $1 $3 }
--     | Expr1 ',' Expr                   { C.Pair $1 $3 }

-- perform applications
Expr1 :: { C.Expr }
Expr1 : Expr2 { let (f : args) = reverse $1 in
                if null args then f else C.App f args 
	      }
       | coset Expr3                      { C.CoSet $2 }
       | set                              { C.Set C.Zero }
       | set Expr3                        { C.Set $2 }
       | number '*' Expr1                 { let n = read $1 in
                                            if n==0 then C.Zero else
                                            iterate (C.Plus $3) $3 !! (n-1) }
       | Domain '&' Expr1                 { C.Quant A.Sigma $1 $3 } 

-- gather applications
Expr2 :: { [C.Expr] }
Expr2 : Expr3 { [$1] }
       | Expr2 Expr3 { $2 : $1 }
       | Expr2 '.' Id { C.Proj $3 : $1 }
       | Expr2 set   { C.Set C.Zero : $1 }
--       | succ SE { [C.Succ $2] }

-- atoms
Expr3 :: { C.Expr }
Expr3 : size                      { C.Size }
      | max                       { C.Max }
      | infty                     { C.Infty }
      | Id                        { C.Ident $1}
      | '<' ExprT ':' Expr '>'     { C.Sing $2 $4 }
      | '(' ExprT ')'              { $2 }
      | '_'                       { C.Unknown }
      | succ Expr3                { C.Succ $2 }  -- succ is a prefix op
      | number                    { iterate C.Succ C.Zero !! (read $1) }
      | record '{' RecordDefs '}' { C.Record $3 }

{-
-- general form of type expression
Type :: { C.Expr }
Type : Domain '->' Type                 { C.Quant A.Pi $1 $3 } 
     | let LBind '=' ExprT in Type      { C.LLet $2 $4 $6 }
     | case ExprT '{' Cases '}'         { C.Case $2 $4 }  
     | Type1                            { $1 }

-- perform applications
Type1 :: { C.Expr }
Type1 : Type2 { let (f : args) = reverse $1 in
                if null args then f else C.App f args 
	      }
       | coset Expr3                      { C.CoSet $2 }
       | set                              { C.Set C.Zero }
       | set Expr3                        { C.Set $2 }
       | Domain '&' Type1                 { C.Quant A.Sigma $1 $3 } 

-- gather applications
Type2 :: { [C.Expr] }
Type2 : Type3 { [$1] }
      | Type2 Expr3 { $2 : $1 }
      | Type2 '.' Id { C.Proj $3 : $1 }
      | Type2 set   { C.Set C.Zero : $1 }

-- type atoms
Type3 :: { C.Expr }
Type3 : size                      { C.Size }
      | Id                        { C.Ident $1}
      | '(' Type ')'              { $2 }
      | '_'                       { C.Unknown }
-}

RecordDefs :: { [([Name],C.Expr)] }
RecordDefs 
  : RecordDef ';' RecordDefs   { $1 : $3 }
  | RecordDef                  { [$1] }           
  | {- empty -}                { [] }

RecordDef :: { ([Name],C.Expr) }
RecordDef : SpcIds '=' ExprT    { ($1,$3) } 

{- RETIRED
SE :: { C.Expr}
SE : succ SE { C.Succ $2}
SE : Expr3 { $1}
  -}

TypeSig :: { C.TypeSig }
TypeSig : Id ':' Expr { C.TypeSig $1 $3 }

Constructor :: { C.Constructor }
Constructor : TypeSig { $1 }

Constructors :: { [C.Constructor ] }
Constructors :
      Constructors ';' Constructor { $3 : $1 }
    | Constructors ';' { $1 }
    | Constructor { [$1] }
    | {- empty -} { [] }

Cases :: { [C.Clause] }
Cases : Pattern '->' ExprT ';' Cases  { (C.Clause Nothing [$1] (Just $3)) : $5 }
      | Pattern '->' ExprT            { (C.Clause Nothing [$1] (Just $3)) : [] }
      | Pattern ';' Cases             { (C.Clause Nothing [$1] Nothing) : $3 }
      | Pattern                       { (C.Clause Nothing [$1] Nothing) : [] }
      | {- empty -}                   { [] }
      
Clause :: { C.Clause }
Clause : Id LHS '=' ExprT { C.Clause (Just $1) $2 (Just $4) }
       | Id LHS           { C.Clause (Just $1) $2 Nothing }

LHS :: { [C.Pattern] }
LHS : Patterns { reverse $1 }

Patterns :: { [C.Pattern] }
Patterns : {- empty -} { [] }
--    | Pattern Patterns { $1 : $2 }
    | Patterns Pattern { $2 : $1 }

-- atomic patterns
Pattern :: { C.Pattern }
Pattern : '(' ')'            { C.AbsurdP     }
        | '(' PairP ')'      { $2            }
        | Id                 { C.IdentP $1   }
        | succ Pattern       { C.SuccP $2    }
        | '.' set            { C.DotP (C.Set C.Zero) }
        | '.' Expr3          { C.DotP $2     }

-- pattern tuples
PairP :: { C.Pattern }
PairP : ElemP ',' PairP     { C.PairP $1 $3 }
      | ElemP               { $1 }

ElemP :: { C.Pattern }
ElemP : ConP                { let (c, ps) = $1 in C.ConP c (reverse ps) }
      | Id '>' Id           { C.SizeP $1 $3 } 
      | Id '<' Id           { C.SizeP $3 $1 } 
      | Pattern             { $1 }

ConP :: { (Name, [C.Pattern]) }
ConP : Id Pattern          { ($1, [$2]) }
     | ConP Pattern        { let (c, ps) = $1 in (c, $2 : ps) }

{-
Pattern :: { C.Pattern }
Pattern : ConP               { $1            }
        | Id                 { C.IdentP $1   }
        | '.' set            { C.DotP (C.Set C.Zero) }
        | '.' Expr3          { C.DotP $2     }
        | '(' Id '>' Id ')'  { C.SizeP $2 $4 } 
        | '(' Id '<' Id ')'  { C.SizeP $4 $2 } 
--        | '*'       { C.AbsurdP }

ConP :: { C.Pattern }
ConP : '(' Id Patterns ')' { C.ConP $2 (reverse $3) }
     | '(' succ SP ')'     { C.SuccP $3             }
     | '(' Pa
     | '(' ')'             { C.AbsurdP              }

SP :: { C.Pattern}
SP : succ SP {C.SuccP $2}
   | Pattern ',' Pattern  { C.PairP $1 $3 }
   | Pattern { $1 }
-}

Clauses :: { [C.Clause] }
Clauses : RClauses { reverse $1 }

RClauses :: { [C.Clause ] }
RClauses :
   RClauses ';' Clause { $3 : $1 }
 | RClauses ';' { $1 }
 | Clause { [$1] }
 | {- empty -} { [] }

-- for backwards compatibility
TBindSP :: { C.TBind }
TBindSP : '(' Ids ':' Expr ')' { C.TBind A.paramDec $2 $4 } -- ordinary binding
        | '[' Ids ':' Expr ']' { C.TBind A.irrelevantDec $2 $4 }  -- erased binding
        | Pol '(' Ids ':' Expr ')' { C.TBind (Dec $1) $3 $5 } 
--        | Pol '[' Ids ':' Expr ']' { C.TBind (Dec True $1) $3 $5 }  
        |  '(' '+' Ids ':' Expr ')' { C.TBind (Dec SPos) $3 $5 }
--        |  '[' '+' Ids ':' Expr ']' { C.TBind (Dec True SPos) $3 $5 }
        | '(' sized Id ')'     { C.TSized $3 }

DataTelescope :: { C.Telescope }
DataTelescope :  {- empty -} { [] }
--                | TBind   DataTelescope { $1 : $2 } 
                | TBindSP DataTelescope { $1 : $2 } 

{

parseError :: [T.Token] -> a
parseError [] = error "Parse error at EOF"
parseError (x : xs) = error ("Parse error at token " ++ T.prettyTok x) 

}

 



