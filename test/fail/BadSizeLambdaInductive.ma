-- 2013-03-30

-- inductive counterexample

data Unit { unit }

data Nat +(i : Size)
{ zero [j < i]
; suc  [j < i] (n : Nat j) }

fun apply : [i : Size] -> ([j < i] -> Nat $j -> Unit) -> Nat i -> Unit
{ apply i f (zero j)  = f j (zero j)
; apply i f (suc j x) = f j (suc j x)
}

fun caseN : [i : Size] -> Unit -> (Nat i -> Unit) -> Nat $i -> Unit
{ caseN i z s (zero j)  = z
; caseN i z s (suc j x) = s x
}

-- not strongly normalizing:
fun run : [i : Size] -> |i| -> Nat i -> Unit
{ run i = apply i (\ j -> caseN j unit (run j))
}
