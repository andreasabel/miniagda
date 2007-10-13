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

eval const list : SList Enum # = cons Enum # aa (cons Enum # bb (cons Enum # cc (nil Enum #))) 

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

eval const revlist : SList Enum # = rev # Enum list

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


data Bool : Set 
{
  tt : Bool;
  ff : Bool
}

fun ite : (A : Set ) -> Bool -> A -> A -> A
{
ite A tt x y = x;
ite A ff x y = y
}

-- merge sort

fun split : (A : Set) -> 
            (i : Size) -> SList A i -> Prod (SList A i)
{
split .A .($ i) (nil A i) = prod (SList A ($ i)) (nil A i) (nil A i);

splt .A .($ ($ i)) (cons .A .($ i) a (nil A i)) = prod (SList A ($ ($ i))) (cons A ($ i) a (nil A i)) (nil A i);

split .A .($ ($ i)) (cons A .($ i) a (cons .A i b as)) = prod (SList A ($( $ i)))
	(cons A ($ i) a (pr1 (SList A (i)) (split A i as)))
	(cons A ($ i) b (pr2 (SList A (i)) (split A i as)))
}

fun merge : (A : Set) -> (leq : A -> A -> Bool) 
            -> SList A # -> SList A # -> SList A #
{
merge .A leq (nil A .#) ys = ys;

merge .A leq (cons A .# x xs) (nil .A .#) = (cons A # x xs);

merge .A leq (cons A .# x xs) (cons .A .# y ys) = ite (SList A #)
	(leq x y) (cons A # x (cons A # y (merge A leq xs ys)))
		  (cons A # y (cons A # x (merge A leq xs ys)))     
}

fun msort : (A : Set) -> (leq : A -> A -> Bool) ->
            (i : Size) -> SList A i -> SList A #
{
  msort .A leq .($ j) (nil A j) = nil A # ;

  msort .A leq .($ ($ i)) (cons A ($ i) a (nil .A .i)) = (cons A ($ i) a (nil A i)) ;

  msort .A leq .($ ($ i)) (cons A ($ i) a (cons .A .i b as)) =
    merge A leq (msort A leq ($ i) (cons A i a (pr1 (SList A i) (split A i as))))
                (msort A leq ($ i) (cons A i b (pr2 (SList A i) (split A i as))))

}

