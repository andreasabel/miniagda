{

module Lexer where

}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

$white+				;
"--".*				;


data				{ tok (\p s -> Data p) }
codata				{ tok (\p s -> CoData p) }
fun				{ tok (\p s -> Fun p) }
cofun				{ tok (\p s -> CoFun p) }
norec				{ tok (\p s -> NoRec p) }
const				{ tok (\p s -> Const p) }
eval				{ tok (\p s -> Eval p)}
mutual				{ tok (\p s -> Mutual p) }
Set				{ tok (\p s -> Set p) }

Size				{ tok (\p s -> Size p) }
\#				{ tok (\p s -> Infty p) }
\$				{ tok (\p s -> Succ p) }

\{				{ tok (\p s -> BrOpen p) }
\}				{ tok (\p s -> BrClose p) }
\(				{ tok (\p s -> PrOpen p) }
\)				{ tok (\p s -> PrClose p) }
\;				{ tok (\p s -> Sem p) }
\:				{ tok (\p s -> Col p) }
\.				{ tok (\p s -> Dot p) }
"->"				{ tok (\p s -> Arrow p)  }
=				{ tok (\p s -> Eq p) }
\\				{ tok (\p s -> Lam p) }

[$alpha $digit \_ \']+		{ tok (\p s -> (Id s p )) }
	

{
data Token = Id String AlexPosn
           | Data AlexPosn
	   | CoData AlexPosn
	   | Mutual AlexPosn
           | Fun AlexPosn
           | CoFun AlexPosn
	   | NoRec AlexPosn
           | Const AlexPosn
           | Set AlexPosn 
	   | Eval AlexPosn
           -- size type
           | Size AlexPosn
           | Infty AlexPosn
           | Succ AlexPosn
           --
           | BrOpen AlexPosn
           | BrClose AlexPosn
           | PrOpen AlexPosn
           | PrClose AlexPosn
           | Sem AlexPosn
           | Col AlexPosn
	   | Dot AlexPosn
           | Arrow AlexPosn
           | Eq AlexPosn
           | Lam AlexPosn
           | NotUsed AlexPosn -- so happy doesn't generate overlap case pattern warning
             deriving (Eq)

prettyTok :: Token -> String
prettyTok c = "\"" ++ tk ++ "\" at " ++ (prettyAlexPosn pos) where   
  (tk,pos) = case c of 
    (Id s p) -> (show s,p)
    Data p -> ("data",p)
    CoData p -> ("codata",p)
    Mutual p -> ("mutual",p)
    Fun p -> ("fun",p)
    CoFun p -> ("cofun",p)
    NoRec p -> ("norec",p)
    Const p -> ("const",p)
    Eval p -> ("eval",p)
    Set p -> ("Set",p)
    Size p -> ("Size",p)
    Infty p -> ("#",p)
    Succ p -> ("$",p)
    BrOpen p -> ("{",p)
    BrClose p -> ("}",p)
    PrOpen p -> ("(",p)
    PrClose p -> (")",p)
    Sem p -> (";",p)
    Col p -> (":",p)
    Dot p -> (".",p)
    Arrow p -> ("->",p)
    Eq p -> ("=",p)
    Lam p -> ("\\",p)
    _ -> error "not used"    


prettyAlexPosn (AlexPn _ line row) = "line " ++ show line ++ ", row " ++ show row

tok f p s = f p s

}