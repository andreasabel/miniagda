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

let list : List Enum = cons aa (cons bb (cons cc (nil ))) 
mutual 
{

	fun rev : ( A : Set ) -> List A  -> List A 
	{

	rev A (nil ) = nil ;
	rev A (cons x xs) = cons (rev1 A x xs) (rev2 A x xs)

	}

	fun rev1 : ( A : Set ) -> A -> List A -> A
	{

	rev1 A a (nil ) = a; 
	rev1 A a (cons x xs) = rev1 A x xs

	}

	fun rev2 : (A : Set ) -> A -> List A -> List A 
	{

	rev2 A a (nil ) = nil ;
	rev2 A a (cons x xs) = rev A (cons a (rev A (rev2 A x xs)))	
	}
}

let revlist : List Enum = rev Enum list

fun flat : (A : Set ) -> List (List A) -> List A
{
flat A (nil  .(List A)) = nil;
flat A (cons .(List A) (nil) yl) = flat A yl;
flat A (cons .(List A) (cons x xl) yl)  = cons x (flat A (cons xl yl))
}

