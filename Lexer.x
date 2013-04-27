
{

module Lexer where

}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$u     = [ . \n ]               -- universal: any character
@ident = $alpha ($alpha | $digit | \_ | \')*  -- identifier

tokens :-

$white+				;
"--".*				;
"{-" ([$u # \-] | \- [$u # \}])* ("-")+ "}" ;


sized	    	     	   	{ tok (\p s -> Sized p) }
data				{ tok (\p s -> Data p) }
codata				{ tok (\p s -> CoData p) }
record				{ tok (\p s -> Record p) }
fields                          { tok (\p s -> Fields p) }
fun				{ tok (\p s -> Fun p) }
cofun				{ tok (\p s -> CoFun p) }
pattern                         { tok (\p s -> Pattern p) }
case                            { tok (\p s -> Case p) }
def				{ tok (\p s -> Def p) }
let				{ tok (\p s -> Let p) }
in				{ tok (\p s -> In p) }
eval				{ tok (\p s -> Eval p)}
fail				{ tok (\p s -> Fail p)}
check				{ tok (\p s -> Check p)}
trustme				{ tok (\p s -> TrustMe p)}
impredicative                 	{ tok (\p s -> Impredicative p)}
mutual				{ tok (\p s -> Mutual p) }
Type				{ tok (\p s -> Type p) }
Set				{ tok (\p s -> Set p) }
CoSet				{ tok (\p s -> CoSet p) }
"<|"                            { tok (\p s -> LTri p) }
"|>"                            { tok (\p s -> RTri p) }
Size				{ tok (\p s -> Size p) }
\#				{ tok (\p s -> Infty p) }
\$				{ tok (\p s -> Succ p) }
max                             { tok (\p s -> Max p) }

\{				{ tok (\p s -> BrOpen p) }
\}				{ tok (\p s -> BrClose p) }
\[				{ tok (\p s -> BracketOpen p) }
\]				{ tok (\p s -> BracketClose p) }
\(				{ tok (\p s -> PrOpen p) }
\)				{ tok (\p s -> PrClose p) }
\|				{ tok (\p s -> Bar p) }
\;				{ tok (\p s -> Sem p) }
\:				{ tok (\p s -> Col p) }
\,				{ tok (\p s -> Comma p) }
\.				{ tok (\p s -> Dot p) }
\+\+                            { tok (\p s -> PlusPlus p) }
\+				{ tok (\p s -> Plus p) }
\-				{ tok (\p s -> Minus p) }
\/				{ tok (\p s -> Slash p) }
\*				{ tok (\p s -> Times p) }
\^				{ tok (\p s -> Hat p) }
\&				{ tok (\p s -> Amp p) }
"->"				{ tok (\p s -> Arrow p)  }
"<="                            { tok (\p s -> Leq p)  }
=				{ tok (\p s -> Eq p) }
\\				{ tok (\p s -> Lam p) }
\_				{ tok (\p s -> Underscore p) }
\<                              { tok (\p s -> AngleOpen p) }
\>                              { tok (\p s -> AngleClose p) }

[$digit]+		        { tok (\p s -> (Number s p )) }
@ident                          { tok (\p s -> (Id s p )) }
@ident \. @ident                { tok (\p s -> (qualId s p)) }

{
data Token = Id String AlexPosn
           | QualId (String, String) AlexPosn
     	   | Number String AlexPosn
     	   | Sized AlexPosn
           | Data AlexPosn
	   | CoData AlexPosn
	   | Record AlexPosn
	   | Fields AlexPosn
	   | Mutual AlexPosn
           | Fun AlexPosn
           | CoFun AlexPosn
           | Pattern AlexPosn
	   | Case AlexPosn
	   | Def AlexPosn
	   | Let AlexPosn
	   | In AlexPosn
           | Type AlexPosn
           | Set AlexPosn
           | CoSet AlexPosn
	   | Eval AlexPosn
	   | Fail AlexPosn
	   | Check AlexPosn
	   | TrustMe AlexPosn
	   | Impredicative AlexPosn
           -- size type
           | Size AlexPosn
           | Infty AlexPosn
           | Succ AlexPosn
           | Max AlexPosn
           --
           | LTri AlexPosn
           | RTri AlexPosn
           | AngleOpen AlexPosn
           | AngleClose AlexPosn
           | BrOpen AlexPosn
           | BrClose AlexPosn
           | BracketOpen AlexPosn
           | BracketClose AlexPosn
           | PrOpen AlexPosn
           | PrClose AlexPosn
           | Bar AlexPosn
           | Sem AlexPosn
           | Col AlexPosn
	   | Comma AlexPosn
	   | Dot AlexPosn
           | Arrow AlexPosn
           | Leq AlexPosn
           | Eq AlexPosn
	   | PlusPlus AlexPosn
	   | Plus AlexPosn
	   | Minus AlexPosn
	   | Slash AlexPosn
	   | Times AlexPosn
	   | Hat AlexPosn
	   | Amp AlexPosn
           | Lam AlexPosn
           | Underscore AlexPosn
           | NotUsed AlexPosn -- so happy doesn't generate overlap case pattern warning
             deriving (Eq)

qualId s p = let (m, '.':n) = break (== '.') s in QualId (m,n) p

prettyTok :: Token -> String
prettyTok c = "\"" ++ tk ++ "\" at " ++ (prettyAlexPosn pos) where
  (tk,pos) = case c of
    (Id s p) -> (show s,p)
    (QualId (m, n) p) -> (show m ++ "." ++ show n, p)
    (Number i p) -> (i,p)
    Sized p -> ("sized",p)
    Data p -> ("data",p)
    CoData p -> ("codata",p)
    Record p -> ("record",p)
    Fields p -> ("fields",p)
    Mutual p -> ("mutual",p)
    Fun p -> ("fun",p)
    CoFun p -> ("cofun",p)
    Pattern p -> ("pattern",p)
    Case p -> ("case",p)
    Def p -> ("def",p)
    Let p -> ("let",p)
    In p -> ("in",p)
    Eval p -> ("eval",p)
    Fail p -> ("fail",p)
    Check p -> ("check",p)
    TrustMe p -> ("trustme",p)
    Impredicative p -> ("impredicative",p)
    Type p -> ("Type",p)
    Set p -> ("Set",p)
    CoSet p -> ("CoSet",p)
    Size p -> ("Size",p)
    Infty p -> ("#",p)
    Succ p -> ("$",p)
    Max p -> ("max",p)
    LTri p -> ("<|",p)
    RTri p -> ("|>",p)
    AngleOpen p -> ("<",p)
    AngleClose p -> (">",p)
    BrOpen p -> ("{",p)
    BrClose p -> ("}",p)
    BracketOpen p -> ("[",p)
    BracketClose p -> ("]",p)
    PrOpen p -> ("(",p)
    PrClose p -> (")",p)
    Bar p -> ("|",p)
    Sem p -> (";",p)
    Col p -> (":",p)
    Comma p -> (",",p)
    Dot p -> (".",p)
    Arrow p -> ("->",p)
    Leq p -> ("<=",p)
    Eq p -> ("=",p)
    PlusPlus p -> ("++",p)
    Plus p -> ("+",p)
    Minus p -> ("-",p)
    Slash p -> ("/",p)
    Times p -> ("*",p)
    Hat p -> ("^",p)
    Amp p -> ("&",p)
    Lam p -> ("\\",p)
    Underscore p -> ("_",p)
    _ -> error "not used"


prettyAlexPosn (AlexPn _ line row) = "line " ++ show line ++ ", row " ++ show row

tok f p s = f p s

}
