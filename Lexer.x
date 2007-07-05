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
fun				{ tok (\p s -> Fun p) }
def				{ tok (\p s -> Def p) }
mutual				{ tok (\p s -> Mutual p) }

Set				{ tok (\p s -> Set p) }

Size				{ tok (\p s -> Size p) }
infty				{ tok (\p s -> Infty p) }
s				{ tok (\p s -> Succ p) }

\{				{ tok (\p s -> BrOpen p) }
\}				{ tok (\p s -> BrClose p) }
\(				{ tok (\p s -> PrOpen p) }
\)				{ tok (\p s -> PrClose p) }
\;				{ tok (\p s -> Sem p) }
\:				{ tok (\p s -> Col p) }
"->"				{ tok (\p s -> Arrow p)  }
=				{ tok (\p s -> Eq p) }
\\				{ tok (\p s -> Lam p) }
_				{ tok (\p s -> UScore p) }

$alpha [$alpha $digit \_ \']*		{ tok (\p s -> (Id s p )) }
	

{
data Token = Id String AlexPosn
           | Data AlexPosn
           | Fun AlexPosn
           | Def AlexPosn
           | Mutual AlexPosn
           | Set AlexPosn 
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
           | Arrow AlexPosn
           | Eq AlexPosn
           | Lam AlexPosn
           | UScore AlexPosn
           | NotUsed AlexPosn -- so happy doesn't generate overlap case pattern warning
             deriving (Eq)

prettyTok :: Token -> String
prettyTok c = "\"" ++ tk ++ "\" at " ++ (prettyAlexPosn pos) where   
  (tk,pos) = case c of 
    (Id s p) -> (show s,p)
    Data p -> ("data",p)
    Fun p -> ("fun",p)
    Def p -> ("def",p)
    Mutual p -> ("mutual",p)
    Set p -> ("set",p)
    Size p -> ("Size",p)
    Infty p -> ("Infty",p)
    Succ p -> ("s",p)
    BrOpen p -> ("{",p)
    BrClose p -> ("}",p)
    PrOpen p -> ("(",p)
    PrClose p -> (")",p)
    Sem p -> (";",p)
    Col p -> (":",p)
    Arrow p -> ("->",p)
    Eq p -> ("=",p)
    Lam p -> ("\\",p)
    UScore p -> ("_",p)
    _ -> error "not used"    


prettyAlexPosn (AlexPn _ line row) = "line " ++ show line ++ ", row " ++ show row



tok f p s = f p s

}