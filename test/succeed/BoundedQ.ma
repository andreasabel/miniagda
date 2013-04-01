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

fun mySucc : [i : Size] -> [j < i] -> Nat j -> Nat i
{ mySucc i j n = succ j n  }

let boundedId [i : Size] [j <= i] (n : Nat j) : Nat j = n

let explicitCast : [i : Size] -> [j <= i] -> Nat j -> Nat i
  = \ i j n -> n

fun explicitCast' : [i : Size] -> [j : Size] -> |j| <= |i| -> Nat j -> Nat i
{ explicitCast' i j n = n
}
