-- 2010-10-01
-- zero # : Nat # ?= zero i : Nat $i  succeeds
-- even though Nat # /= Nat $i

sized data Nat : Size -> Set
{ zero : [i : Size] -> Nat $i
; succ : [i : Size] -> Nat i -> Nat $i
}

fun good :
  [Size] ->
  [f : [i : Size] -> Nat i -> Set] ->
  (g : [i : Size] -> (n : Nat i) -> f i n) ->
  (h : f # (zero #) -> Set) ->
  Set
{ good i f g h = h (g $i (zero i))
}

{- f # (zero #) : Set  >=  f $i (zero i) : Set
   zero #     : Nat #  ?=  zero i     : Nat $i
-}

