data Nat : Set
{
	zero : Nat;
	succ : Nat -> Nat
}

data List : Nat -> Set
{
	nil2 : List zero
}

data Vec (A : Set) : Nat -> Set
{
	nil : Vec A zero;
	cons : (n : Nat ) -> A -> Vec A n -> Vec A (succ n)
}

fun head : (A: Set) -> (n : Nat ) -> Vec A (succ n) -> A
{
head A n (cons A n a xl) = a
}

fun zeroes : ( n : Nat ) -> Vec Nat n
{

zeroes zero = nil Nat;
zeroes (succ x) = cons Nat x zero (zeroes x)
}

const four : Nat = succ (succ (succ (succ zero)))
const five : Nat = succ four

const headzeroes5 : Nat = head Nat four (zeroes five)

