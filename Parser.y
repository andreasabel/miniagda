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
fun     { T.Fun _ }
cofun   { T.CoFun _ } 
const   { T.Const _ }
mutual  { T.Mutual _ }
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
'->'    { T.Arrow _ }
'='     { T.Eq _ }
'\\'     { T.Lam _ }
'_'     { T.UScore _ } 

%%

TopLevel :: { [A.Declaration] }
TopLevel : Declarations { reverse $1}

Declaration :: { A.Declaration }
Declaration : Definition { let (tsl,dl) = $1 in A.Declaration [tsl] [dl]  }
              | mutual '{' Definitions '}' { let (tsl,dl) = $3 in A.Declaration (reverse tsl) (reverse dl) }



Declarations :: { [A.Declaration] }
Declarations : {- empty -} { [] }
             | Declarations Declaration { $2 : $1 }


Definitions :: { ([A.TypeSig],[A.Definition]) }
Definitions : Definition { let (ts,d) = $1 in ([ts],[d]) }
            | Definitions Definition { let (tsl,dl) = $1 in let (ts,d) = $2 in (ts:tsl,d:dl) }

Definition :: { (A.TypeSig,A.Definition) }
Definition : Data { $1 }
           | CoData { $1 }
           | Fun { $1 }
           | CoFun { $1 }
           | Const { $1 }

Data :: { (A.TypeSig,A.Definition) }
Data : data Id Telescope ':' Expr '{' Constructors '}' 
       { (A.TypeSig $2 $5,A.DataDef A.Ind $3 (reverse $7)) }

CoData :: { (A.TypeSig,A.Definition) }
CoData : codata Id Telescope ':' Expr '{' Constructors '}' 
       { (A.TypeSig $2 $5,A.DataDef A.CoInd $3 (reverse $7)) }

Fun :: { (A.TypeSig,A.Definition) }
Fun : fun TypeSig '{' Clauses '}' { ($2 , A.FunDef A.Ind (reverse $4)) }

CoFun :: { (A.TypeSig,A.Definition) }
CoFun : cofun TypeSig '{' Clauses '}' { ($2 , A.FunDef A.CoInd (reverse $4)) }

Const :: { (A.TypeSig,A.Definition) }
Const : const TypeSig '=' Expr { ($2,A.ConstDef $4) } 


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
Patterns : Pattern { [$1] }
    | Patterns Pattern { $2 : $1 }

Pattern :: { A.Pattern }
Pattern : '_' { A.WildP }
        | '(' ')' { A.AbsurdP }
        | ConP { $1 }
        | Id { A.IdentP $1 }

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

 



