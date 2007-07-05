data SList ( A : Set ) : Size -> Set 
{

nil : (i : Size ) -> SList A (s i) ;
cons : (i : Size ) -> A -> SList A i -> SList A (s i)

}

fun weakList : ( i : Size ) -> ( A : Set ) -> SList A i -> SList A (s i)
{

i A (nil i) = nil (s i);
(s i) A (cons i x xs) = cons (s i) (x (weakList i A xs))

}

mutual 
{

	fun rev : ( i : Size ) -> ( A : Set ) -> SList A i -> SList A i
	{

	(s i) A (nil (s i)) = nil (s i) ;
	(s i) A (cons (s i) x xs) = cons (s i) (rev1 i A x xs) (rev2 i A x xs)

	}


	fun rev1 : ( i : Size ) -> ( A : Set ) -> A -> SList A i -> A
	{

	(s i) A a (nil (s i)) = a ;
	(s i) A a (cons (s i) x xs) = rev1 i A x xs

	}



	fun rev2 : ( i : Size ) -> (A : Set ) -> A -> SList A i -> SList A i
	{

	(s i) A a (nil (s i)) = (nil (s i));
	(s i) A a (cons (s i) x xs) = rev (s i) A (cons (s i) a (rev i A (rev2 i A x xs)))	
	}

}
