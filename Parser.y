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
Data : data Id DataTelescope ':' Expr '{' Constructors '}' 
   { let (pos,tel) = unzip $3 
       in A.DataDecl $2 A.Ind pos tel $5 (reverse $7) }

CoData :: { A.Declaration }
CoData : codata Id DataTelescope ':' Expr '{' Constructors '}' 
       { let (pos,tel) = unzip $3 
         in A.DataDecl $2 A.CoInd pos tel $5 (reverse $7) }

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
Const : const TypeSig '=' Expr { A.ConstDecl False $2 $4 } 
      | eval const TypeSig '=' Expr { A.ConstDecl True $3 $5 } 

Id :: { A.Name }
Id : id { $1 }
     
Expr :: { A.Expr }
Expr : 
       TArrow Expr { let (n,t) = $1 in A.Pi n t $2 } 
       | '\\' Id '->' Expr { A.Lam $2 $4 }
       | Expr1 '->' Expr { A.Pi "" $1 $3}
       | Expr1 { $1 }

TArrow :: { (A.Name,A.Expr) }
TArrow : TBind '->' { $1 }  

Expr1 :: { A.Expr }
Expr1 : Expr2 { let list = reverse $1 in
               if length list == 1 then head list else A.App (head list) (tail list) 
	      }
--      | SizeE { $1}
Expr2 :: { [A.Expr] }
Expr2 : Expr3 { [$1] }
       | succ SE { [A.Succ $2] }
       | Expr2 Expr3 { $2 : $1 }

Expr3 :: { A.Expr }
Expr3 :  set { A.Set}
      | size { A.Size }
      | infty { A.Infty }
      | Id { A.Ident $1}
      | '(' Expr ')' { $2 }

SE :: { A.Expr}
SE : succ SE { A.Succ $2}
SE : Expr3 { $1}

TypeSig :: { A.TypeSig }
TypeSig : Id ':' Expr { A.TypeSig $1 $3 }

Constructor :: { A.Constructor }
Constructor : TypeSig { $1 }

Constructors :: { [A.Constructor ] }
Constructors :
      Constructors ';' Constructor { $3 : $1 }
    | Constructors ';' { $1 }
    | Constructor { [$1] }
    | {- empty -} { [] }


Clause :: { A.Clause }
Clause : Id LHS '=' Expr { A.Clause $2 $4 }

LHS :: { [A.Pattern] }
LHS : Patterns { reverse $1 }

Patterns :: { [A.Pattern] }
Patterns : {- empty -} { [] }
    | Patterns Pattern { $2 : $1 }

Pattern :: { A.Pattern }
Pattern : ConP { $1 }
        | Id { A.IdentP $1 }
        | '.' Expr3 { A.DotP $2 }

ConP :: { A.Pattern }
ConP : '(' Id Patterns ')' { A.ConP A.NN $2 (reverse $3) }
     | '(' succ SP ')' { A.SuccP $3 }

SP :: { A.Pattern}
SP : succ SP {A.SuccP $2}
   | Pattern { $1 }
Clauses :: { [A.Clause ] }
Clauses :
   Clauses ';' Clause { $3 : $1 }
 | Clauses ';' { $1 }
 | Clause { [$1] }
 | {- empty -} { [] }


TBind :: { (A.Name,A.Expr) }
TBind :  '(' Id ':' Expr ')' { ($2,$4) }

TBindSP :: { (A.Pos,(A.Name,A.Expr)) }
TBindSP :  '(' '+' Id ':' Expr ')' { (A.SPos,($3,$5)) }

TBindNP :: { (A.Pos,(A.Name,A.Expr)) }
TBindNP :  '(' Id ':' Expr ')' { (A.NSPos,($2,$4)) }

DataTelescope :: { [(A.Pos,(A.Name,A.Expr))] }
DataTelescope : {- empty -} { [] }
                | TBindSP DataTelescope { $1 : $2 } 
                | TBindNP DataTelescope { $1 : $2 } 

{

parseError :: [T.Token] -> a
parseError [] = error "Parse error at EOF"
parseError (x : xs) = error ("Parse error at token " ++ T.prettyTok x) 

}

 



