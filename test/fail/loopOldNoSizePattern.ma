sized data SNat : Size -> Set
{
  zero : (i : Size ) -> SNat ($ i);
  succ : (i : Size) -> SNat i -> SNat ($ i)
}

data Maybe (+ A : Set) : Set
{
  nothing : Maybe A;
  just : A -> Maybe A
}

let Nat : Set = SNat #

fun shift_case : (i : Size) -> Maybe (SNat ($ i)) -> 
                               Maybe (SNat i)
{
  shift_case  i (nothing .(SNat ($ i)))         = nothing (SNat i);
  shift_case .i (just .(SNat ($ i)) (zero i))   = nothing (SNat i);
  shift_case .i (just .(SNat ($ i)) (succ i x)) = just (SNat i) x  
}

let shift : (i : Size) -> (Nat -> Maybe (SNat ($ i))) -> 
                           Nat -> Maybe (SNat i) = 
\i -> \f -> \n -> shift_case i (f (succ # n))

let inc : Nat -> Maybe Nat = \n -> just Nat (succ # n)

data Unit : Set
{
  unit : Unit
}

mutual 
{
  
  fun loop : (i : Size) -> SNat i -> (Nat -> Maybe (SNat i)) -> Unit
  {
    loop .($ i) (zero i)   f = loop_case ($ i) f (f (zero i)); 
    loop .($ i) (succ i n) f = loop i n (shift i f)
  }
  
  fun loop_case : (i : Size) -> (Nat -> Maybe (SNat i)) -> 
                                Maybe (SNat i) -> Unit
  {
    loop_case i       f (nothing .(SNat i)) = unit;
    loop_case .($ i)  f (just .(SNat ($ i)) (zero i)) = unit;
    loop_case .($ i)  f (just .(SNat ($ i)) (succ i y)) = loop i y (shift i f) 
  }
}

eval let diverge : Unit = loop # (zero #) inc