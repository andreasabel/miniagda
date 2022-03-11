sized data SNat : Size -> Set
{
        zero : (i : Size) -> SNat ($ i);
        succ : (i : Size) -> SNat i -> SNat ($ i)
}

let z : SNat # = zero #
let one : SNat # = succ # z
let two : SNat # = succ # one
let three : SNat # = succ # two


-- 2010-08-18 all these functions fail because ($ i) is restricted to cofun

fun add : (i : Size) -> (j : Size) -> SNat i -> SNat j -> SNat #
{

add ($ i) j (zero .i) y = y;
add ($ i) j (succ .i x) y = succ # (add i j x y)

}

let four : SNat # = add # # two two
let six : SNat # = add # # four two

fun minus : (i : Size) -> (j : Size) -> SNat i -> SNat j -> SNat i
{

minus ($ i) ($ j)  (zero .i)    y           = zero i;
minus ($ i) ($ j)  x            (zero .j)  = x ;
minus ($ i) ($ j)  (succ .i x)  (succ .j y) = minus i j x y

}

let min4_2 : SNat # = minus # #  four two

-- not structurally recursive without sizes ...
fun div : (i : Size) -> (j : Size) ->  SNat i -> SNat j -> SNat i
{

div ($ i) ($ j)  (zero .i)   y = (zero i) ;
div ($ i) ($ j)  x           (zero .j) = (zero i);
div ($ i) ($ j)  (succ .i x) (succ .j y) = succ i (div i ($ j) (minus i j x y) (succ j y))

}

let div4_4 : SNat # = div # # four four


fun compare : (i : Size) -> (j : Size) -> (SNat i) -> (SNat j)
    -> (A : Set) -> A -> A -> A
{
compare ($ i) ($ j) x (zero .j)                   A a a' = a ;
compare ($ i) ($ j) (zero .i) (succ .j y')        A a a' = a';
compare ($ i) ($ j) (succ .i x) (succ .j y)       A a a' = compare i j x y A a a'
}

fun gcd : (i : Size) -> (j : Size) -> (SNat i) -> (SNat j) -> (SNat #)
{
gcd ($ i)  j    (zero .i)    y         = y ;
gcd  i    ($ j)  x         (zero .j)   = x ;
gcd ($ i) ($ j) (succ .i x) (succ .j y) =
    compare i j x y (SNat #)
               (gcd i ($ j) (minus i j x y) (succ j y))
               (gcd ($ i) j (succ i x) (minus j i y x))
}

let gcd6_4 : SNat # = gcd # # six four

data SEmpty : Size -> Set
{
}

fun bad : (i : Size) -> SNat i -> SEmpty i
{
bad i x = x
}

fun bad2 : (A : Set) -> (B : Set) -> A -> B
{
bad2 A B x = x
}
