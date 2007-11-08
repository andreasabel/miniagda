{ 
module Parser where

import qualified Lexer as T
import qualified Concrete as C

import qualified Abstract as A
import Abstract (Name)
}

%name parse
%tokentype { T.Token }
%error { parseError }

%token

id      { T.Id $$ _ }
data    { T.Data _ }
codata  { T.CoData _ }
mutual  { T.Mutual _ }
fun     { T.Fun _ }
cofun   { T.CoFun _ } 
const   { T.Const _ }
eval    { T.Eval _ }
set     { T.Set _ }
size    { T.Size _ }
infty   { T.Infty _ }
succ    { T.Succ _ }
'{'     { T.BrOpen _ }
'}'     { T.BrClose _ }
'('     { T.PrOpen _ }
')'     { T.PrClose _ }
';'     { T.Sem _ }
':'     { T.Col _ }
'.'     { T.Dot _ }
'->'    { T.Arrow _ }
'='     { T.Eq _ }
'+'     { T.Plus _ }
'\\'    { T.Lam _ }

%%

TopLevel :: { [C.Declaration] }
TopLevel : Declarations { reverse $1}


Declarations :: { [C.Declaration] }
Declarations : {- empty -} { [] }
             | Declarations Declaration { $2 : $1 }

Declaration :: { C.Declaration }
Declaration : Data { $1 }
           | CoData { $1 }
           | Fun { $1 }
           | CoFun { $1 }
           | Mutual { $1 }
           | Const { $1 }

Data :: { C.Declaration }
Data : data Id DataTelescope ':' Expr '{' Constructors '}' 
   { C.DataDecl $2 A.Ind $3 $5 (reverse $7) }

CoData :: { C.Declaration }
CoData : codata Id DataTelescope ':' Expr '{' Constructors '}' 
       { C.DataDecl $2 A.CoInd $3 $5 (reverse $7) }

Fun :: { C.Declaration }
Fun : fun TypeSig '{' Clauses '}' { C.FunDecl A.Ind $2 $4 }

CoFun :: { C.Declaration }
CoFun : cofun TypeSig '{' Clauses '}' { C.FunDecl A.CoInd $2 $4  }



Mutual :: { C.Declaration }
Mutual : mutual '{' Declarations '}' { C.MutualDecl (reverse $3) }
     

 
Const :: { C.Declaration }
Const : const TypeSig '=' Expr { C.ConstDecl False $2 $4 } 
      | eval const TypeSig '=' Expr { C.ConstDecl True $3 $5 } 

-----

Id :: { Name }
Id : id { $1 }
     
Expr :: { C.Expr }
Expr : 
       TArrow Expr { let (n,t) = $1 in C.Pi n t $2 } 
       | '\\' Id '->' Expr { C.Lam $2 $4 }
       | Expr1 '->' Expr { C.Pi "" $1 $3}
       | Expr1 { $1 }

TArrow :: { (Name,C.Expr) }
TArrow : TBind '->' { $1 }  

Expr1 :: { C.Expr }
Expr1 : Expr2 { let list = reverse $1 in
               if length list == 1 then head list else C.App (head list) (tail list) 
	      }

Expr2 :: { [C.Expr] }
Expr2 : Expr3 { [$1] }
       | succ SE { [C.Succ $2] }
       | Expr2 Expr3 { $2 : $1 }

Expr3 :: { C.Expr }
Expr3 :  set { C.Set}
      | size { C.Size }
      | infty { C.Infty }
      | Id { C.Ident $1}
      | '(' Expr ')' { $2 }

SE :: { C.Expr}
SE : succ SE { C.Succ $2}
SE : Expr3 { $1}

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


Clause :: { C.Clause }
Clause : Id LHS '=' Expr { C.Clause $2 $4 }

LHS :: { [C.Pattern] }
LHS : Patterns { reverse $1 }

Patterns :: { [C.Pattern] }
Patterns : {- empty -} { [] }
    | Patterns Pattern { $2 : $1 }

Pattern :: { C.Pattern }
Pattern : ConP { $1 }
        | Id { C.IdentP $1 }
        | '.' Expr3 { C.DotP $2 }

ConP :: { C.Pattern }
ConP : '(' Id Patterns ')' { C.ConP $2 (reverse $3) }
     | '(' succ SP ')' { C.SuccP $3 }

SP :: { C.Pattern}
SP : succ SP {C.SuccP $2}
   | Pattern { $1 }

Clauses :: { [C.Clause] }
Clauses : RClauses { reverse $1 }

RClauses :: { [C.Clause ] }
RClauses :
   RClauses ';' Clause { $3 : $1 }
 | RClauses ';' { $1 }
 | Clause { [$1] }
 | {- empty -} { [] }


TBind :: { (Name,C.Expr) }
TBind :  '(' Id ':' Expr ')' { ($2,$4) }

TBindSP :: { C.TBind }
TBindSP :  '(' '+' Id ':' Expr ')' { C.PosTB $3 $5 }

TBindNP :: { C.TBind }
TBindNP :  '(' Id ':' Expr ')' { C.TB $2 $4 }

DataTelescope :: { C.Telescope }
DataTelescope :  {- empty -} { [] }
                | TBindSP DataTelescope { $1 : $2 } 
                | TBindNP DataTelescope { $1 : $2 } 

{

parseError :: [T.Token] -> a
parseError [] = error "Parse error at EOF"
parseError (x : xs) = error ("Parse error at token " ++ T.prettyTok x) 

}

 



