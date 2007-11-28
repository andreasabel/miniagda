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

fun add : ( i : Size) -> SNat i -> SNat # -> SNat #
{

add .($ i) (zero i) y = y; 
add .($ i) (succ i x) y = succ # (add i x y) 

}

eval let four : SNat # = add # two two
let six : SNat # = add # four two

fun minus : (i : Size ) -> SNat i -> SNat # -> SNat i
{

minus .($ i) (zero i)    y          = zero i;
minus i      x           (zero .#)  = x ;
minus .($ i) (succ i x)  (succ .# y) = minus i x y

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
               (gcd i ($ j) (minus i x y) (succ j y))         
               (gcd ($ i) j (succ i x) (minus j y x))
}

let gcd6_4 : SNat # = gcd # # six four

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
let add' : Nat -> Nat -> Nat = add #

let plus_1_0_is_1 : Eq Nat (add' one z) one = refl Nat one

  