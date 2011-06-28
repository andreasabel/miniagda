{- 2010-03-24 Awaji Island

Mixed coinduction/induction.  Allow data with coinductive occurrences.
Interpreted as greatest fixpoint of a least fixpoint.
-}

sized codata Str (+ A : Set) : Size -> Set
{ cons : [i : Size] -> A -> Str A i -> Str A ($ i)
}

fun A : Set {}
fun B : Set {}

sized data SP' (+ X : Set) : Size -> Set 
{ get : [j : Size] -> (A -> SP' X j) -> SP' X ($ j)
; out : [j : Size] -> X -> SP' X ($ j)
}

sized codata SP : Size -> Set 
{ put : [i : Size] -> B -> SP' (SP i) # -> SP ($ i)
}

fun run' : [i : Size] -> (SP i -> Str A # -> Str B i) ->
           [j : Size] -> SP' (SP i) j -> Str A # -> Str B i
{ run' i r j (get .(SP i) (j > k) f) (cons .A .# a as) = run' i r k (f a) as
; run' i r j (out .(SP i) (j > k) sp) as            = r sp as
}

cofun run : [i : Size] -> SP i -> Str A # -> Str B i
{ run ($ i) (put .i b sp) as  = cons B i b (run' i (run i) # sp as)
}


