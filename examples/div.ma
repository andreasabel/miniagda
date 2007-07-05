-- sized natural numbers

data SNat : Size -> Set 
{

	succ : ( i : Size ) -> SNat i -> SNat (s i);
	zero : ( i : Size ) -> SNat (s i)
}

def Nat : Set = SNat infty

fun weakNat : Set
{

i       (zero i) = (zero (s i));
(s i)   (succ x) = succ (weakNat i (succ x)) 

}

fun minus : (i : Size ) -> (j : Size ) -> (SNat i) -> (SNat j) -> (SNat i)
{

i j           zero     x     = zero ;
i j	      x        zero  = x ;
(s i) (s j)  (succ x)  (succ y) = weakNat i (minus i j x y)

}

-- not structurally recursive without sizes ... 
fun div : ( i : Size ) -> ( j : Size ) ->  (SNat (s i)) -> (SNat j) -> (SNat (s i))
{

i     j  zero     _ = zero ;
(s i) j  (succ x) y = succ (div i j (minus i j x y) y)

}
