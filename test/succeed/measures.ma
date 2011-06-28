-- 2010-03-11 explicit measures
-- inspired by Hongwei Xi, LICS 2001

data Bool : Set
{ true : Bool
; false : Bool
}

sized data Nat : Size -> Set
{ zero : [i : Size] -> Nat ($ i)
; succ : [i : Size] -> Nat i -> Nat ($ i)
}

{- rules

2010-07-16

When checking the patterns ps qs of f

  f : As -> mu -> Bs -> C
  f ps qs = ... g as ...
  
as we reach measure mu in the type, insert it into the context 
(reader monad) as the current measure. 

When we reach a mutually defined identifier g of type
  
  g : Delta -> mu' -> D

we use it at type  

 g : Delta -> mu' < mu -> D

The constraint mu' < mu guarantees that g is only called at smaller instances.

How to implement this?

When checking a mutual block where all identifiers carry a measure
(either all should be measured, or none (then use old termination
check)), we keep the signature of the mutual block around

  g1 : TV1
  ...
  gn : TVn

The types are evaluated.  We want a function
  
  bound :: MeasVal -> TVal -> TVal
  bound mu tv = tv'

such that

  bound mu (Delta -> mu' -> A) = Delta -> mu' < mu -> A

this can done lazyly, pushing the mu past the pi's until it meets the
measure.  If there is no measure, then it just gets propagated to the
end and vanishes.  This way, we can handle call to rec. fun.s outside
of the mutual block which can be used unrestrictedly.

If we have a mu-decoration to values, then we need a view function
which does the pushing in behind the curtains.  This could be
integrated in the whnf and closures.

We do not need to keep "bound-closures" at unevaluated applications,
since we do not treat measures as first-class, the cannot appear
everywhere in a term, only in the types of a mutual fun sig, so they
are just appearing after a telescope.

-- old rules ---------------------------------------------------------

When checking the patterns ps qs of f

  f : As -> mu -> Bs -> C
  f ps qs = ... g as ...
  
as we reach measure mu in the type, insert it into the context 
(reader monad) as the current measure. 

When checking the application g as on the rhs, if g is in the set of
mutual functions with f, then during infering the type of g as we will
have its type as

   mu' -> D

at some point.  Then, we simply check whether

  mu' < mu

Yeah!

Typing rules for measures

  |-{mu}  t : A             |-{mu}  t : mu' -> A    mu' < mu
  -------------- mu-Intro   -------------------------------- mu-Elim
  |- t : mu -> A            |-{mu}  t : A
  
After finishing checking the mutual block, purge the measures from the
types of the mutual functions!

For nested functions, generalize the rule to:

  |-{mu}  t : A           
  ------------------- mu-Intro 
  |-{mu'} t : mu -> A          
  
With this system, one cannot do lexicographic induction by nested induction.


Explicit rules for measures ?
                                                        mu subtype mu'
  x : mu |- t : A             x : mu |- t : mu' -> A    mu' < mu
  -------------- mu-Intro     ---------------------------------- mu-Elim
  |- \xt : mu -> A            x : mu |- t x : A
  
 -}

mutual {

  fun even  : [i : Size] -> |i,$0| -> Nat i -> Bool
  { even i n = even' i n
  }

  fun even' : [i : Size] -> |i,0|  -> Nat i -> Bool
  { even' i (zero (i > j))   = true
  ; even' i (succ (i > j) n) = odd' j n
  } 

  fun odd'  : [i : Size] -> |i,0|  -> Nat i -> Bool
  { odd' i (zero (i > j))   = false
  ; odd' i (succ (i > j) n) = even j n
  } 
}

{-
mutual {

  fun even  : [i : Size] -> |i,$0| -> Nat i -> Bool
  { even i n = even' i n
  }

  fun even' : [i : Size] -> |i,0|  -> Nat i -> Bool
  { even' .($ i) (zero i)   = true
  ; even' .($ i) (succ i n) = odd' i n
  } 

  fun odd'  : [i : Size] -> |i,0|  -> Nat i -> Bool
  { odd' .($ i) (zero i)   = false
  ; odd' .($ i) (succ i n) = even i n
  } 
}
-}


{-
let infty : Size = #
let ssuc : Size -> Size = \ i -> $ i

fun maybeSuc : (b : Bool) -> Size -> Size
{ maybeSuc true i = $ i
; maybeSuc false i = i
}

fun addSize : N -> Size -> Size
{ addSize zz i = i
; addSize (ss n) i = $ (addSize n i)
}

fun addSNat : (n : N) -> (i : Size) -> Nat i -> Nat (addSize n i)
{ addSNat zz     i m = m
; addSNat (ss n) i m = succ (addSize n i) (addSNat n i m) 
}
-}
