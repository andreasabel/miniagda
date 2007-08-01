data Nat : Set  
{
	zero : Nat ;
	succ : Nat -> Nat
}

const one : Nat = succ zero
const two : Nat = succ one

fun add : Nat -> Nat -> Nat 
{

add x zero = x ;
add x (succ y) = succ (add x y)

}

fun add' : Nat -> Nat -> Nat
{
zero = \ (y : Nat) -> y ;
(succ x) = \ (y : Nat) -> succ (add' x y)
}

const plus1 : Nat -> Nat = add' one
const one'  : Nat = plus1 zero
const zero' : Nat = add' zero zero
const three : Nat = add' one two

const plus1 : Nat -> Nat = add one

const three : Nat = plus1 two

fun mult : Nat -> Nat -> Nat
{
mult x zero = zero ;
mult x (succ y) = add x (mult x y)
}

const four : Nat = add two two

const eight : Nat = mult four two

fun fak : Nat -> Nat
{
fak zero = one;
fak (succ x) = mult (succ x) (fak x)
}

const fak4 : Nat = fak four 
