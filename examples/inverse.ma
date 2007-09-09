data Nat : Set
{
zero : Nat;
succ : Nat -> Nat
}

fun f : Nat -> Nat
{
f zero = zero;
f (succ x) = succ x
}

data Imf : Nat -> Set
{
imf : (x : Nat ) -> Imf (f x)
}

--right inverse
fun invf : (y : Nat ) -> Imf y -> Nat
{
invf .(f x) (imf x) = x 
}