-- 2010-07-01 Ana asked whether nesting is possible

sized data Nat : Size -> Set
{ zero : [i : Size] -> Nat $i
; succ : [i : Size] -> Nat i -> Nat $i
}

fun nested : [i : Size] -> Nat i -> Nat i
{ nested i (zero (i > j))   = zero j
; nested i (succ (i > j) n) = nested j (nested j n)
}