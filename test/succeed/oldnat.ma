-- Mugda (Karl Mehltretter's master thesis)
-- sized natural numbers

sized data SNat : Size -> Set
{
zero : [i : Size] -> SNat ($ i);
succ : [i : Size] -> SNat i -> SNat ($ i)
}

fun add : SNat # -> SNat # -> SNat #
{
add (zero .#)   y = y; 
add (succ .# x) y = succ # (add x y) 
}

fun inc : (i : Size) -> (j : Size) -> SNat i -> SNat ($ i)
{
inc i j x = succ _ x;
}

fun minus : [i : Size] -> SNat i -> SNat # -> SNat i
{
minus .($ i) (zero i)   y           = zero _;
minus i      x          (zero .#)   = x;
minus .($ i) (succ i x) (succ .# y) = minus _ x y    -- subtyping i < ($ i)
}

eval let test : SNat # = 
  minus # (succ # (succ # (zero #))) (succ # (zero #))

-- div n m = floor(n/(m+1)) 
fun div : [i : Size] -> SNat i -> SNat # -> SNat i
{
div .($ i) (zero i)   y = zero _ ;
div .($ i) (succ i x) y = succ _ (div _ (minus _ x y) y)
}

data Bool : Set
{
  tt : Bool;
  ff : Bool
}

fun true : [i : Size] -> SNat i -> Bool
{
true .($ i) (zero i) = tt;
true .($ i) (succ i x) = true _ x
}

-- ok size variable is a valid pattern

fun ok : Size -> Bool
{
  ok i = tt
}
