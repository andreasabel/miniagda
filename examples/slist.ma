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




data Prod (A : Set) : Set 
{
  prod : A -> A -> Prod A 
}

fun pr1 : ( A : Set ) -> Prod A -> A
{
pr1 .A (prod A a b) = a
}

fun pr2 : ( A : Set ) -> Prod A -> A
{
pr2 .A (prod A a b) = b
}

-- merge sort

fun split : (A : Set) -> (leq : A -> A -> Set) ->
            (i : Size) -> SList A i -> Prod (SList A i)
{
}

fun merge : (A : Set) -> (leq : A -> A -> Set) ->
            (i : Size) -> SList A i -> (j : Size) -> SList A j -> SList A #
{
}

fun msort : (A : Set) -> (leq : A -> A -> Set) ->
            (i : Size) -> SList A i -> SList A #
{
  msort .A leq ($ .i) (nil A i) = nil A i ;
  msort .A leq ($ ($ .i)) (cons .A .($ i) a (nil A i)) = (cons A ($ i) a (nil A i)) ;
  msort .A leq ($ ($ .i)) (cons .A .($ i) a (cons A i b as)) =
    (\ r -> merge A leq # (msort A leq ($ i) (cons A i a (pr1 (SList A i) r)))
            merge A leq # (msort A leq ($ i) (cons A i b (pr2 (SList A i) r)))
    ) (split A leq i as)
}

