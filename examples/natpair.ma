data Nat : Set
{
zero : Nat;
succ : Nat -> Nat
}

fun add : Nat -> Nat -> Nat
{
add x zero = x;
add x (succ y) = succ (add y x)
}

{-

data Pair : Set
{
pair : Nat -> Nat -> Pair  
}



fun add : Pair -> Nat
{
add (pair x zero) = x;
add (pair x (succ y)) = succ (add (pair y x))
}

fun ack : Pair -> Nat
{
ack (pair zero y) = y;
ack (pair (succ x) zero) = ack (pair x (succ zero));
ack (pair (succ x) (succ y)) = ack (pair x (ack (pair (succ x) y)))
}

-- should fail
fun foo : Pair -> Nat
{
foo (pair zero x) = zero;
foo (pair (succ x) zero) = foo (pair (succ x) (succ x));
foo (pair (succ x) (succ y)) = foo (pair (succ y) (succ y))
}


-}