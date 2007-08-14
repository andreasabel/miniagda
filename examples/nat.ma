data Nat : Set  
{
	zero : Nat ;
	succ : Nat -> Nat
}


fun id : Nat -> Nat
{
id x = x 
}

const one : Nat = id (succ zero)

fun add : Nat -> Nat -> Nat 
{
add x zero = x ;
add x (succ y) = succ (add x y) 
}

const three : Nat = add (succ (succ zero)) (succ (zero))

