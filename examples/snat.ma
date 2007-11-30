sized data SNat : Size -> Set
{
	zero : (i : Size ) -> SNat ($ i);
	succ : (i : Size ) -> SNat i -> SNat ($ i)
}

let z : SNat # = zero #
eval let one : SNat # = succ # z
let two : SNat # = succ # one
let three : SNat # = succ # two

fun wkSNat : (i : Size ) -> SNat i -> SNat ($ i)
{
wkSNat .($ i) (zero i) = zero ($ i);
wkSNat .($ i) (succ i x) = succ ($ i) (wkSNat i x) 
}

-- bad  , incomplete pattern
fun wkSNat2 : (i : Size ) -> SNat ($ i) -> SNat i
{
wkSNat2 .($ i) (zero ($ i)) = zero i;
wkSNat2 .($ i) (succ ($ i) x) = succ i (wkSNat2 i x) 
}

fun wkNatInfty : (i : Size) -> SNat i -> SNat #
{
wkNatInfty .($ i) (zero i) = zero #;
wkNatInfty .($ i) (succ i n) = succ # (wkNatInfty i n)
}

fun add : SNat # -> SNat # -> SNat #
{

add (zero .#) y = y; 
add (succ .# x) y = succ # (add x y) 

}

eval let four : SNat # = add two two
let six : SNat # = add four two

fun minus : (i : Size ) -> SNat i -> SNat # -> SNat i
{

minus .($ i) (zero i)    y          = zero i;
minus i      x           (zero .#)  = x ;
minus .($ i) (succ i x)  (succ .# y) = minus i x y --subtyping i < ($ i)

}

let min4_2 : SNat # = minus #  four two

-- not structurally recursive without sizes ... 
fun div : ( i : Size )  ->  SNat i -> SNat # -> SNat i
{
div ($ .i) (zero i)   y            = zero i ;
div ($ .i) (succ i x) (zero .#)    = zero i ;
div ($ .i) (succ i x) (succ .# y)  = succ i (div i (minus i x y) (succ # y))

}

eval let div4_4 : SNat # = div # four four


fun compare : SNat # -> SNat #
    -> (A : Set) -> A -> A -> A
{
compare x           (zero .#)    A a a' = a ;
compare (zero .#)   (succ .# y') A a a' = a';
compare (succ .# x) (succ .# y)  A a a' = compare x y A a a'
}

fun gcd : (i : Size ) -> (j : Size ) -> SNat i -> SNat j -> SNat #
{
   gcd .($ i)  j      (zero i)   y          = y; 
   gcd .($ i)  .($ j) (succ i x) (zero j)   = x ; 
   gcd .($ i)  .($ j) (succ i x) (succ j y) = 
   compare x y (SNat #)
               (gcd i ($ j) (minus i x y) (succ j y))         
               (gcd ($ i) j (succ i x) (minus j y x))
}

eval let gcd6_4 : SNat # = gcd # # two two

data Eq (A : Set) (a : A) : A -> Set 
{
  refl : Eq A a a
}

fun subst : (A : Set) -> (P :  A -> Set) -> (a : A) -> (b : A) -> 
            Eq A a b -> P a -> P b
{
subst .A P .a .a (refl A a) p = p
}

let Nat : Set = SNat #

let plus_1_0_is_1 : Eq Nat (add one z) one = refl Nat one

let wkFun : (i : Size ) -> (SNat i -> SNat i) -> (SNat i -> SNat ($ i)) = \i -> \f -> ( \x -> wkSNat i (f x) )  

