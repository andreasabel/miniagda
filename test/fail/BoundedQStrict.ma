-- 2010-11-12

{-  another way to look at sized types:

sized data Nat (i : Size) : Set
{ zero : Nat i
; succ : [j : Size] -> |j| < |i| -> Nat j -> Nat i
}

-}
sized data Nat : Size -> Set
{ zero : [i : Size] -> Nat $i
; succ : [i : Size] -> Nat i -> Nat $i
}

fun mySucc : [i : Size] -> [j : Size] -> |j| < |i| -> Nat j -> Nat i
{ mySucc i j n = succ j n }

let bla : [i : Size] -> [j : Size] -> |j| <= |i| -> Nat j -> Nat i
  = \ i j n -> mySucc i j n
-- needs to fail
