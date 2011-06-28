sized data SNat : Size -> Set
{ zero : [i : Size] -> SNat ($ i)
; succ : [i : Size] -> SNat i -> SNat ($ i)
}

let Nat : Set = SNat #

data Unit : Set
{ unit : Unit
}

data Maybe (+ A : Set) : Set
{ nothing : Maybe A
; just : A -> Maybe A
}

fun shift_case : [i : Size] -> Maybe (SNat ($ i)) -> Maybe (SNat i)
{ shift_case  i (nothing .(SNat ($ i)))         = nothing (SNat i)
; shift_case .i (just .(SNat ($ i)) (zero i))   = nothing (SNat i)
; shift_case .i (just .(SNat ($ i)) (succ i x)) = just (SNat i) x  
}

let shift : [i : Size] -> (Nat -> Maybe (SNat ($ i))) -> 
                           Nat -> Maybe (SNat i) = 
  \i -> \f -> \n -> shift_case i (f (succ # n))

mutual 
{
  
  fun loop : [i : Size] -> SNat i -> (Nat -> Maybe (SNat i)) -> Unit
  { loop i (zero (i > j)  ) f = loop_case i f (f (zero j))
  ; loop i (succ (i > j) n) f = loop j n (shift j f)
  }
  
  fun loop_case : [i : Size] -> (Nat -> Maybe (SNat i)) -> 
                                Maybe (SNat i) -> Unit
  { loop_case i f (nothing .(SNat i)) = unit
  ; loop_case i f (just .(SNat i) (zero (i > j)  )) = unit
  ; loop_case i f (just .(SNat i) (succ (i > j) y)) = loop j y (shift j f) 
      -- f : Nat -> Maybe (SNat i)  should have type  Nat -> Maybe (SNat ($ j))
      -- but we only know $ j <= i  and not equality
  }
}

let inc : Nat -> Maybe Nat = \n -> just Nat (succ # n)

eval let diverge : Unit = loop # (zero #) inc