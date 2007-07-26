data Nat : Set  
{
	zero : Nat ;
	succ : Nat -> Nat
}

const one : Nat = succ zero
const two : Nat = succ one

fun add : Nat -> Nat -> Nat 
{

x zero = x ;
x (succ y) = succ (add x y)

}

fun mult : Nat -> Nat -> Nat
{
x zero = zero ;
x (succ y) = add x (mult x y)
}

const four : Nat = add two two

const eight : Nat = mult four two

fun fak : Nat -> Nat
{
zero = one;
(succ x) = mult (succ x) (fak x)
}

const fak4 : Nat = fak four 
