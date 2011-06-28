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

fail
let mySucc : [i : Size] -> [j : Size] -> |j| < |i| -> Nat i -> Nat j
 = \ i j n -> succ j n 

let explicitCast : [i : Size] -> [j : Size] -> |j| <= |i| -> Nat i -> Nat j
  = \ i j n -> n
