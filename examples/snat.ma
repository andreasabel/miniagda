data SNat : Size -> Set
{
	zero : (i : Size ) -> SNat (s i);
	succ : (i : Size ) -> SNat i -> SNat (s i)
}

const z : SNat infty = zero infty
const one : SNat infty = succ infty z
const two : SNat infty = succ infty one
const three : SNat infty = succ infty two


fun wkSNat : (i : Size ) -> SNat i -> SNat (s i)
{
wkSNat (s i) (zero .i) = zero (s i);
wkSNat (s i) (succ .i x) = succ (s i) (wkSNat i x) 
}

fun wkNatInfty : (i : Size) -> SNat i -> SNat infty
{
wkNatInfty (s i) (zero .i) = zero infty;
wkNatInfty (s i) (succ .i n) = succ infty (wkNatInfty i n)
}

fun add : ( i : Size) -> ( j : Size ) -> SNat i -> SNat j -> SNat infty
{

add (s i) j (zero .i) y = wkNatInfty j y; 
add (s i) j (succ .i x) y = succ infty (add i j x y) 

}

const four : SNat infty = add infty infty two two
const six : SNat infty = add infty infty four two

fun minus : (i : Size ) -> (j : Size ) -> SNat i -> SNat j -> SNat i
{

minus (s i) (s j)  (zero .i)    y           = zero i;
minus (s i) (s j)  x            (zero .j )  = x ;
minus (s i) (s j)  (succ .i x)  (succ .j y) = wkSNat i (minus i j x y)

}

const min4_2 : SNat infty = minus infty infty  four two

-- not structurally recursive without sizes ... 
fun div : ( i : Size ) -> ( j : Size ) ->  SNat i -> SNat j -> SNat i
{

div (s i) (s j)  (zero .i)   y = (zero i) ;
div (s i) (s j)  x           (zero .j) = (zero i);
div (s i) (s j)  (succ .i x) (succ .j y) = succ i (div i (s j) (minus i j x y) (succ j y))

}

const div4_4 : SNat infty = div infty infty four four


fun compare : (i : Size ) -> (j : Size ) -> (SNat i) -> (SNat j)
    -> (A : Set) -> A -> A -> A
{
compare (s i) (s j) x (zero .j)                   A a a' = a ;
compare (s i) (s j) (zero .i) (succ .j y')        A a a' = a';
compare (s i) (s j) (succ .i x) (succ .j y)       A a a' = compare i j x y A a a'
}

fun gcd : (i : Size ) -> (j : Size ) -> (SNat i) -> (SNat j) -> (SNat infty)
{
gcd (s i)  j    (zero .i)    y         = wkNatInfty j y ;
gcd  i    (s j)  x         (zero .j)   = wkNatInfty i x ;
gcd (s i) (s j) (succ .i x) (succ .j y) = 
    compare i j x y (SNat infty)
               (gcd i (s j) (minus i j x y) (succ j y))
               (gcd (s i) j (succ i x) (minus j i y x))
}

const gcd6_4 : SNat infty = gcd infty infty six four




