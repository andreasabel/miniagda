data Nat : Set
{
zero : Nat;
succ : Nat -> Nat
}

data Pair : Set
{
pair : Nat -> Nat -> Pair  
}

const addn : Nat -> Nat -> Nat = \x -> \y -> x

fun add : Pair -> Nat
{
add (pair x zero) = x;
add (pair x (succ y)) = succ (add (pair x y))
}

fun ack : Pair -> Nat
{
ack (pair zero y) = y;
ack (pair (succ x) zero) = ack (pair x (succ zero))
--ack (pair (succ x) (succ y)) = ack (pair x (ack (pair (succ x) y)))
}

-- should fail
fun foo : Pair -> Nat
{
foo (pair zero x) = zero;
foo (pair (succ x) zero) = foo (pair x (succ (zero)));
foo (pair (succ x) (succ y)) = foo (pair (succ x) (succ y))
}


mutual {

fun f : Nat -> Nat -> Nat
{
  f zero y = zero;
  f (succ x) zero = zero;
  f (succ x) (succ y) = addn (g x (succ y)) (f  (succ (succ x)) y)
}

fun g : Nat -> Nat -> Nat
{
  g zero y = zero;
  g (succ x) zero = zero;
  g (succ x) (succ y) = addn (f (succ x) (succ y)) (g x (succ (succ y)))
}

}
