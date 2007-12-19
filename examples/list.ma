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

let list : List Enum = cons Enum aa (cons Enum bb (cons Enum cc (nil Enum ))) 
mutual 
{

	fun rev : ( A : Set ) -> List A  -> List A 
	{

	rev .A (nil A ) = nil A ;
	rev .A (cons A x xs) = cons A (rev1 A x xs) (rev2 A x xs)

	}

	fun rev1 : ( A : Set ) -> A -> List A -> A
	{

	rev1 .A a (nil A ) = a; 
	rev1 .A a (cons A x xs) = rev1 A x xs

	}

	fun rev2 : (A : Set ) -> A -> List A -> List A 
	{

	rev2 .A a (nil A ) = nil A ;
	rev2 .A a (cons A x xs) = rev A (cons A a (rev A (rev2 A x xs)))	
	}
}

let revlist : List Enum = rev Enum list

fun flat : (A : Set ) -> List (List A) -> List A
{
flat A (nil  .(List A)) = nil A;
flat A (cons .(List A) (nil .A) yl) = flat A yl;
flat A (cons .(List A) (cons .A x xl) yl)  = cons A x (flat A (cons (List A) xl yl))
}

