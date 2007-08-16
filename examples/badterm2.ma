data Nat : Set
{
	zero : Nat;
	succ : Nat -> Nat
}

fun minus : Nat -> Nat -> Nat
{

minus zero y = zero ;
minus x zero = x ;
minus (succ x) (succ y) = minus x y 

}

-- not structurally recursive without sizes ... 
fun div : Nat -> Nat -> Nat
{

div zero y = zero ;
div x zero = zero ;
div (succ x) (succ y) = succ (div (minus x y) (succ y))

}
