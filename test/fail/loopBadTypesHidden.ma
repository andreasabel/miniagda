sized data SNat : Size -> Set
{
        zero : (i : Size) -> SNat ($ i);
        succ : (i : Size) -> SNat i -> SNat ($ i)
}

data Maybe (+ A : Set) : Set
{
  nothing : Maybe A;
  just : A -> Maybe A
}

let Nat : Set = SNat #

fun shift_case : (i : Size) -> Maybe (SNat ($ i)) -> Maybe (SNat i)
{

shift_case i nothing = nothing;
shift_case .i (just (zero i)) = nothing;
shift_case .i (just (succ i x)) = just x

}

let shift : (i : Size) -> (Nat -> Maybe (SNat ($ i))) -> Nat -> Maybe (SNat i) =
\i -> \f -> \n -> shift_case i (f (succ # n))

let inc : Nat -> Maybe Nat = \n -> just (succ # n)

data Unit : Set
{
        unit : Unit
}

fun loopType : Unit -> Set
{
loopType unit = (i : Size) -> SNat i -> (Nat -> Maybe (SNat i)) -> Unit
}

fun loopCaseType : Unit -> Set
{
loopCaseType unit = (i : Size) -> (Nat -> Maybe (SNat i)) -> Maybe (SNat i) -> Unit
}


-- hide bad types ....
mutual
{

fun loop : (u : Unit) -> loopType u
{
loop unit .($ i) (zero i) f = loop_case unit ($ i) f (f (zero i));
loop unit .($ i) (succ i n) f = loop unit i n (shift i f)
}

fun loop_case : (u : Unit) -> loopCaseType u
{
loop_case unit i       f (nothing) = unit;
loop_case unit .($ i)  f (just  (zero i)) = unit;
loop_case unit .($ i)  f (just (succ i y)) = loop unit i y (shift i f)
}
}

eval let diverge : Unit = loop unit # (zero #) inc
