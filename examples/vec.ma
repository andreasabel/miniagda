data Nat : Set
{
	zero : Nat;
	succ : Nat -> Nat
}

fun add : Nat -> Nat -> Nat
{
add zero y = y;
add (succ x) y = succ (add x y) 
}

data Vec (A : Set ) : Nat -> Set
{
	nil  : Vec A zero;
	cons : (n : Nat) -> A -> Vec A n -> Vec A (succ n)
}

norec length : (A : Set) -> (n : Nat) -> Vec A n -> Nat
{
  length A n v = n
}

fun head : (A : Set ) -> (n : Nat ) -> Vec A (succ n) -> A 
{
head .A .m (cons A m x xl) = x
}

fun tail : ( A: Set ) -> (n : Nat) -> Vec A (succ n) -> Vec A n
{
tail .A .m (cons A m x xl) = xl
}

fun zeroes : ( n : Nat ) -> Vec Nat n
{

zeroes zero = nil Nat ;
zeroes (succ x) = cons Nat x zero (zeroes x)
}

fun append : (A : Set ) -> ( n : Nat ) -> ( m : Nat) -> Vec  A n -> Vec A m -> Vec A (add n m)
{
append .A .zero m (nil A) v = v;
append .A .(succ n) m (cons A n x xl) v = cons A (add n m) x (append A n m xl v)
}

mutual 
{

	fun rev : ( n : Nat ) -> ( A : Set ) -> Vec A n -> Vec A n
	{

	rev .zero     .A  (nil A) = nil A;
	rev .(succ n) .A (cons A n x xs) = cons A n (rev1 n A x xs) (rev2 n A x xs)
	}

	fun rev1 : ( n : Nat ) -> ( A : Set ) -> A -> Vec A n -> A
	{

	rev1 .zero     .A a (nil A) = a ;
	rev1 .(succ n) .A a (cons A n x xs) = rev1 n A x xs

	}

	fun rev2 : ( n : Nat ) -> (A : Set ) -> A -> Vec A n -> Vec A n
	{
	rev2 .zero .A  a (nil A) = nil A;
	rev2 .(succ n) .A a (cons A n x xs) = rev (succ n) A (cons A n a (rev n A (rev2 n A x xs)))	
	}

}
