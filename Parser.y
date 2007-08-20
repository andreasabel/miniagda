{ 
module Parser where

import qualified Lexer as T
import qualified Abstract as A

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
'\\'     { T.Lam _ }
'_'     { T.UScore _ } 

%%

TopLevel :: { [A.Declaration] }
TopLevel : Declarations { reverse $1}


Declarations :: { [A.Declaration] }
Declarations : {- empty -} { [] }
             | Declarations Declaration { $2 : $1 }

Declaration :: { A.Declaration }
Declaration : Data { $1 }
           | CoData { $1 }
           | mFun { $1 }
           | mCoFun { $1 }
           | Const { $1 }

Data :: { A.Declaration }
Data : data Id Telescope ':' Expr '{' Constructors '}' 
       { A.DataDecl $2 A.Ind $3 $5 (reverse $7) }

CoData :: { A.Declaration }
CoData : codata Id Telescope ':' Expr '{' Constructors '}' 
       { A.DataDecl $2 A.CoInd $3 $5 (reverse $7) }

Fun :: { (A.TypeSig,[A.Clause]) }
Fun : fun TypeSig '{' Clauses '}' { ($2,(reverse $4)) }

Funs :: { [(A.TypeSig,[A.Clause])] }
Funs : Fun { [$1] }
     | Fun Funs { $1 : $2 }

mFun :: { A.Declaration }
mFun : mutual '{' Funs '}' { A.FunDecl A.Ind $3 }
     | Fun { A.FunDecl A.Ind [$1] }
 

CoFun :: { (A.TypeSig,[A.Clause]) }
CoFun : cofun TypeSig '{' Clauses '}' { ($2,(reverse $4)) }

CoFuns :: { [(A.TypeSig,[A.Clause])] }
CoFuns : CoFun { [$1] }
       | CoFun CoFuns { $1 : $2 }

mCoFun :: { A.Declaration }
mCoFun : mutual '{' CoFuns '}' { A.FunDecl A.CoInd $3 }
       | CoFun { A.FunDecl A.CoInd [$1] }
 



Const :: { A.Declaration }
Const : const TypeSig '=' Expr { A.ConstDecl $2 $4 } 


Id :: { A.Name }
Id : id { $1 }
     
Expr :: { A.Expr }
Expr : 
         TArrow Expr { A.Pi $1 $2 } 
       | '\\' Id '->' Expr { A.Lam $2 $4 }
       | Expr1 '->' Expr { A.Fun $1 $3}
       | Expr1 { $1 }

TArrow :: { A.TBind }
TArrow : TBind '->' { $1 }  

Expr1 :: { A.Expr }
Expr1 : Expr2 { let list = reverse $1 in
               if length list == 1 then head list else A.App (head list) (tail list) 
	      }

Expr2 :: { [A.Expr] }
Expr2 : Expr3 { [$1] }
      | succ Expr3 { [A.Succ $2] }
      | Expr2 Expr3 { $2 : $1 }

Expr3 :: { A.Expr }
Expr3 :  set { A.Set}
      | size { A.Size }
      | infty { A.Infty }
      | Id { A.Ident $1}
      | '(' Expr ')' { $2 }

TypeSig :: { A.TypeSig }
TypeSig : Id ':' Expr { A.TypeSig $1 $3 }

Constructor :: { A.Constructor }
Constructor : TypeSig { $1 }

Constructors :: { [A.Constructor ] }
Constructors : {- empty -} { [] }
         | Constructor { [$1] }
         | Constructors ';' Constructor { $3 : $1 }


Clause :: { A.Clause }
Clause : Id LHS RHS { A.Clause $2 $3 }

LHS :: { A.LHS }
LHS : Patterns { A.LHS (reverse $1) }

Patterns :: { [A.Pattern] }
Patterns : {- empty -} { [] }
    | Patterns Pattern { $2 : $1 }

Pattern :: { A.Pattern }
Pattern : '_' { A.WildP }
        | '(' ')' { A.AbsurdP }
        | ConP { $1 }
        | Id { A.IdentP $1 }
        | '.' Expr3 { A.DotP $2 }

ConP :: { A.Pattern }
ConP : '(' Id Patterns ')' { A.ConP $2 (reverse $3) }
     | '(' succ Pattern ')' { A.SuccP $3 }

RHS :: { A.RHS }
RHS : {- empty -} { A.AbsurdRHS }
    | '=' Expr { A.RHS $2 }
    
Clauses :: { [A.Clause ] }
Clauses : {- empty -} { [] }
        | Clause { [$1] }
| Clauses ';' Clause { $3 : $1 }


TBind :: { A.TBind }
TBind :  '(' Id ':' Expr ')' { A.TBind $2 $4 }


Telescope :: { A.Telescope }
Telescope : {- empty -} { [] }
          | TBind Telescope { $1 : $2 } 


{

parseError :: [T.Token] -> a
parseError [] = error "Parse error at EOF"
parseError (x : xs) = error ("Parse error at token " ++ T.prettyTok x) 

}

 



