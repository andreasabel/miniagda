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

add .(s i) j (zero i) y = y; -- subtyping
add .(s i) j (succ i x) y = succ infty (add i j x y) 

}

const four : SNat infty = add infty infty two two

fun minus : (i : Size ) -> (j : Size ) -> SNat i -> SNat j -> SNat i
{

minus .(s i) j      (zero i)    y           = zero i;
minus .(s i) .(s j) (succ i x)  (zero j )   =  succ i x ;
minus .(s i) .(s j) (succ i x)  (succ j y)  = minus i j x y -- subtyping

}

const min3_1 : SNat infty = minus infty infty  three one


fun compare : (i : Size ) -> (j : Size ) -> (SNat i) -> (SNat j)
    -> (A : Set) -> A -> A -> A
{
compare i      .(s j) x          (zero j)          A a a' = a ;
compare .(s i) .(s j) (zero i)   (succ j y')        A a a' = a';
compare .(s i) .(s j) (succ i x) (succ j y)       A a a' = compare i j x y A a a'
}

fun gcd : (i : Size ) -> (j : Size ) -> (SNat i) -> (SNat j) -> (SNat infty)
{
gcd .(s i)  j      (zero i)   y          = y; 
gcd .(s i)  .(s j) (succ i x) (zero j)   = x ;
gcd .(s i)  .(s j) (succ i x) (succ j y) = 
    compare i j x y (SNat infty)
               (gcd i (s j) (minus i j x y) (succ j y))         
               (gcd (s i) j (succ i x) (minus j i y x))
}

data List ( A : Set ) : Set 
{
nil : List A;
cons : A -> List A -> List A
}

-- would not be needed with higher order subtyping
fun wkNatList : (i : Size ) -> List (SNat i) -> List (SNat (s i))
{
wkNatList i (nil .(SNat i)) = nil (SNat (s i));
wkNatList i (cons .(SNat i) x xl) = cons (SNat (s i)) x (wkNatList i xl) --subtyping for x  
}