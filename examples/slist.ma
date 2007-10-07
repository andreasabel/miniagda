data Enum : Set
{
	aa : Enum ;
	bb : Enum ; 
	cc : Enum 
}

data SList ( A : Set ) : Size -> Set 
{

nil : (i : Size ) -> SList A ($ i) ;
cons : (i : Size ) -> A -> SList A i -> SList A ($ i)

}

const list : SList Enum # = cons Enum # aa (cons Enum # bb (cons Enum # cc (nil Enum #))) 

mutual 
{

	fun rev : ( i : Size ) -> ( A : Set ) -> SList A i -> SList A i
	{

	rev .($ i) .A (nil A i) = nil A i ;
	rev .($ i) .A (cons A i x xs) = cons A i (rev1 i A x xs) (rev2 i A x xs)

	}


	fun rev1 : ( i : Size ) -> ( A : Set ) -> A -> SList A i -> A
	{

	rev1 .($ i) .A a (nil A i) = a ;
	rev1 .($ i) .A a (cons A i x xs) = rev1 i A x xs

	}



	fun rev2 : ( i : Size ) -> (A : Set ) -> A -> SList A i -> SList A i
	{

	rev2 .($ i) .A a (nil A i) = nil A i;
	rev2 .($ i) .A a (cons A i x xs) = rev ($ i) A (cons A i a (rev i A (rev2 i A x xs)))	
	}

}

const revlist : SList Enum # = rev # Enum list

const EnumSList : Size -> Set = \j -> SList Enum j


