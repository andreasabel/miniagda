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