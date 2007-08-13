data Enum : Set
{
	aa : Enum ;
	bb : Enum ; 
	cc : Enum 
}

data SList ( A : Set ) : Size -> Set 
{

nil : (i : Size ) -> SList A (s i) ;
cons : (i : Size ) -> A -> SList A i -> SList A (s i)

}

const list : SList Enum infty = cons Enum infty aa (cons Enum infty bb (cons Enum infty cc (nil Enum infty))) 

fun weakList : ( i : Size ) -> ( A : Set ) -> SList A i -> SList A (s i)
{

weakList (s i) A (nil .A .i) = nil A (s i);
weakList (s i) A (cons .A .i x xs) = cons A (s i) x (weakList i A xs)

}

mutual 
{

	fun rev : ( i : Size ) -> ( A : Set ) -> SList A i -> SList A i
	{

	rev (s i) A (nil .A .i) = nil A i ;
	rev (s i) A (cons .A .i x xs) = cons A i (rev1 i A x xs) (rev2 i A x xs)

	}


	fun rev1 : ( i : Size ) -> ( A : Set ) -> A -> SList A i -> A
	{

	rev1 (s i) A a (nil .A .i) = a ;
	rev1 (s i) A a (cons .A .i x xs) = rev1 i A x xs

	}



	fun rev2 : ( i : Size ) -> (A : Set ) -> A -> SList A i -> SList A i
	{

	rev2 (s i) A a (nil .A .i) = nil A i;
	rev2 (s i) A a (cons .A .i x xs) = rev (s i) A (cons A i a (rev i A (rev2 i A x xs)))	
	}

}

const revlist : SList Enum infty = rev infty Enum list