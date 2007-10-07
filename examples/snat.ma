data SNat : Size -> Set
{
	zero : (i : Size ) -> SNat ($ i);
	succ : (i : Size ) -> SNat i -> SNat ($ i)
}

const z : SNat # = zero #
const one : SNat # = succ # z
const two : SNat # = succ # one
const three : SNat # = succ # two


fun wkSNat : (i : Size ) -> SNat i -> SNat ($ i)
{
wkSNat .($ i) (zero i) = zero ($ i);
wkSNat .($ i) (succ i x) = succ ($ i) (wkSNat i x) 
}

-- bad  , not admissible
--fun wkSNat2 : (i : Size ) -> SNat ($ i) -> SNat i
--{
--wkSNat2 .($ i) (zero ($ i)) = zero i;
--wkSNat2 .($ i) (succ ($ i) x) = succ i (wkSNat2 i x) 
--}

fun wkNatInfty : (i : Size) -> SNat i -> SNat #
{
wkNatInfty .($ i) (zero i) = zero #;
wkNatInfty .($ i) (succ i n) = succ # (wkNatInfty i n)
}

--fun add : ( i : Size) -> SNat i -> SNat # -> SNat #
fun add : ( i : Size) -> ( j : Size ) -> SNat i -> SNat j -> SNat #
{

add .($ i) j (zero i) y = y; 
add .($ i) j (succ i x) y = succ # (add i j x y) 

}

const four : SNat # = add # # two two
const six : SNat # = add # # four two

-- fun minus : (i : Size ) -> SNat i -> SNat # -> SNat i
fun minus : (i : Size ) -> (j : Size ) -> SNat i -> SNat j -> SNat i
{

minus .($ i) j       (zero i)    y           = zero i;
minus i      .($ j)  x           (zero j )  = x ;
minus .($ i) .($ j)  (succ i x)  (succ j y) = minus i j x y

}

const min4_2 : SNat # = minus # #  four two

-- not structurally recursive without sizes ... 
fun div : ( i : Size ) -> ( j : Size ) ->  SNat i -> SNat j -> SNat i
{

div .($ i) j      (zero i)   y           = zero i ;
div .($ i) .($ j) (succ i x) (zero j)    = zero i ;
div .($ i) .($ j) (succ i x) (succ j y)  = succ i (div i ($ j) (minus i j x y) (succ j y))

}

const div4_4 : SNat # = div # # four four


fun compare : (i : Size ) -> (j : Size ) -> (SNat i) -> (SNat j)
    -> (A : Set) -> A -> A -> A
{
compare i      .($ j) x          (zero j)          A a a' = a ;
compare .($ i) .($ j) (zero i)   (succ j y')        A a a' = a';
compare .($ i) .($ j) (succ i x) (succ j y)       A a a' = compare i j x y A a a'
}

fun gcd : (i : Size ) -> (j : Size ) -> SNat i -> SNat j -> SNat #
{
gcd .($ i)  j      (zero i)   y          = y; 
gcd .($ i)  .($ j) (succ i x) (zero j)   = x ;
gcd .($ i)  .($ j) (succ i x) (succ j y) = 
    compare i j x y (SNat #)
               (gcd i ($ j) (minus i j x y) (succ j y))         
               (gcd ($ i) j (succ i x) (minus j i y x))
}


const gcd6_4 : SNat # = gcd # # six four

data Eq (A : Set) (a : A) : A -> Set 
{
  refl : Eq A a a
}

fun subst : (A : Set) -> (P :  A -> Set) -> (a : A) -> (b : A) -> 
            Eq A a b -> P a -> P b
{
subst .A P .a .a (refl A a) p = p
}

const Nat : Set = SNat #
const add' : Nat -> Nat -> Nat = add # #
const plus_1_0_is_1 : Eq Nat (add' one z) one = refl Nat one

  