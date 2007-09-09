data Nat : Set
{
	zero : Nat;
	succ : Nat -> Nat
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

const four : Nat = succ (succ (succ (succ zero)))
const five : Nat = succ four

const headzeroes5 : Nat = head four (zeroes five)

