-- 2010-05-11

data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

sized codata Stream (+ A : Set) : Size -> Set
{ cons : [i : Size] -> (head : A) -> (tail : Stream A i) -> Stream A ($ i)
}
fields head, tail

fun guard : [j : Size] -> (Stream Nat ($ j) -> Stream Nat #)
                       -> (Stream Nat j     -> Stream Nat #)
{ guard j g xs = g (cons j zero xs)
}

-- the type of f is not admissible
cofun f : [i : Size] -> (Stream Nat i -> Stream Nat #) -> Stream Nat i
{ f ($ j) g = guard j g (f j (guard j g))
}

-- LOOP!
eval let loop : Nat = head # (f # (tail #))

{-
-- the type of f is not admissible
cofun f : (Stream Nat # -> Stream Nat #) ->
  [i : Size] -> (Stream Nat i -> Stream Nat #) -> Stream Nat i
{ f h ($ j) g = h (g (cons j zero
    (f (\ x -> h (h x))
       j
       (\ x -> g (cons j zero x)))))
}

-}
