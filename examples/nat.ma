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

data Eq ( A : Set ) : A -> A -> Set
{
refl : (a : A) -> Eq A a a 
}

--const proof : (x : Nat ) -> Eq Nat (add x zero) x = \ y -> refl Nat y  

-- does not type check
--const proof2 : ( x : Nat ) -> Eq Nat (add zero x) x = \ y -> refl Nat y

fun eqsucc : (x : Nat ) -> (y : Nat ) -> Eq Nat x y -> Eq Nat (succ x) (succ y)
{
eqsucc .x .x (refl .Nat x) = refl Nat (succ x)
}

fun proof2 : ( x : Nat ) -> Eq Nat (add zero x) x
{
proof2 zero = refl Nat zero;
proof2 (succ x) = eqsucc (add zero x) x (proof2 x)
} 