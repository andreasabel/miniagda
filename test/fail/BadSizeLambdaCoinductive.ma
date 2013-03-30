-- 2013-03-30 illegal size lambda \ j < i in rhs

-- coinductive counterexample

data S -(i : Size) { inn (out : [j < i] -> S j) }
fields out

fun eta : [i : Size] -> ([j < i] -> S $j) -> S i
{ eta i f .out j = f j .out j
}

let cons [i : Size] (s : S i) : S $i
  = inn (\ j -> s)

-- not strongly normalizing:
fun inf : [i : Size] -> |i| -> S i
{ inf i = eta i (\ j -> cons j (inf j))
}
