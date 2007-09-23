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

data List : Nat -> Set
{
	nil2 : List zero
}

data Vec : Nat -> Set
{
	nil : Vec zero;
	cons : (y : Nat ) -> Nat -> Vec y -> Vec (succ y)
}


fun head : (n : Nat ) -> Vec (succ n) -> Nat 
{
head .m (cons m x xl) = x
}

fun tail : (n : Nat) -> Vec (succ n) -> Vec n
{
tail .m (cons m x xl) = xl
}

fun zeroes : ( n : Nat ) -> Vec n
{

zeroes zero = nil ;
zeroes (succ x) = cons x zero (zeroes x)
}

fun append : ( n : Nat ) -> ( m : Nat) -> Vec n -> Vec m -> Vec (add n m)
{
append .zero m nil v = v;
append .(succ n) m (cons n x xl) v = cons (add n m) x (append n m xl v)
}