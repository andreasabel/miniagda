data SNat : Set 
{
	zero : ( i : Size ) -> (SNat i);
	succ : ( i : Size ) -> (SNat i) -> (SNat (s i))
}

const z : SNat infty = zero infty
const one : SNat infty = succ infty z
const two : SNat infty = succ infty one
const three : SNat infty = succ infty two

fun wkNat : (i : Size) -> (SNat i) -> (SNat (s i))
{
wkNat i (zero a) = zero (s a)
}

fun wkNatInfty : (i : Size) -> (SNat i) -> (SNat infty)
{
wkNatInfty (s i) (zero i) = zero infty;
wkNatInfty (s i) (succ i n) = succ infty (wkNatInfty i n)
}

const zw : SNat infty = wkNat infty z

fun add : ( i : Size) -> ( j : Size ) -> (SNat i) -> (SNat j) -> (SNat infty)
{

add i     j (zero a) y = y ;
add (s i) j (succ a x) y = succ infty (add i j x y) 

}

const four : SNat infty = add infty infty two two


fun minus : (i : Size ) -> (j : Size ) -> (SNat i) -> (SNat j) -> (SNat i)
{

minus i j          (zero a)  y        = zero a;
minus i j	     x         (zero a) = x ;
minus (s i) (s j)  (succ a x)  (succ b y) = (minus i j x y)

}

const min4_2 : SNat infty = minus infty infty  four two

-- not structurally recursive without sizes ... 
fun div : ( i : Size ) -> ( j : Size ) ->  (SNat (s i)) -> (SNat (s j)) -> (SNat (s i))
{

div i     j  (zero a)   y = zero a ;
div i     j  x	    (zero a) = zero a;
div (s i) j  (succ a x) (succ b y) = succ a (div i j (minus i j x y) (succ b y))

}

const div4_1 : SNat infty = div infty infty four one

fun compare : (i : Size ) -> (j : Size ) -> (SNat i) -> (SNat j)
   -> (A : Set) -> A -> A -> A
{
compare i (succ j) x (zero j)                   A a a' = a;
compare (succ i) (succ j) (zero i) (succ j y')  A a a' = a';
compare (succ i) (succ j) (succ i x) (succ j y) A a a' =
  compare i j x y A a a'
}

fun gcd : (i : Size ) -> (j : Size ) -> (SNat i) -> (SNat j) -> (SNat infty)
{
gcd (s i)  j    (zero i)    y         = wkNatInfty j y ;
gcd  i    (s j)  x         (zero j)   = wkNatInfty i x ;
gcd (s i) (s j) (succ i x) (succ j y) = 
  compare i j x y (SNat infty)
                  (gcd i (s j) (minus i j x y) (succ j y))
                  (gcd (s i) j (succ i x) (minus j i y x))
}
