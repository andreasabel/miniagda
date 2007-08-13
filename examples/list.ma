data Enum : Set
{
	aa : Enum ;
	bb : Enum ; 
	cc : Enum 
}

data List ( A : Set ) : Set 
{

nil : List A;
cons : A -> List A -> List A   
}

fun isNil : ( A : Set) -> List A -> Enum
{
isNil A (nil .A) = bb;
isNil A (cons .A a l) = cc
}