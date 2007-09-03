data SNat : Size -> Set
{
	zero : (i : Size ) -> SNat (s i);
	succ : (i : Size ) -> SNat i -> SNat (s i)
}

const z : SNat infty = zero infty
const one : SNat infty = succ infty z
const two : SNat infty = succ infty one
const three : SNat infty = succ infty two

fun add : ( i : Size) -> ( j : Size ) -> SNat i -> SNat j -> SNat infty
{

add (s i) j (zero .i) y = y; -- without weakening 
add (s i) j (succ .i x) y = succ infty (add i j x y) 

}

const four : SNat infty = add infty infty two two

fun minus : (i : Size ) -> (j : Size ) -> SNat i -> SNat j -> SNat i
{

minus (s i) (s j)  (zero .i)    y           = zero i;
minus (s i) (s j)  x            (zero .j )  = x ;
minus (s i) (s j)  (succ .i x)  (succ .j y) = minus i j x y -- without weakening

}

const min3_1 : SNat infty = minus infty infty  three one
