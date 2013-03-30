-- 2013-03-30 ICFP 2013 paper

data Unit { unit }

-- primitive counterexample

fun sabotage : [i : Size] -> ([j < i] -> Unit) -> Unit
{ sabotage i f = unit
}

-- not strongly normalizing
fun wtf : [i : Size] -> |i| -> Unit
{ wtf i = sabotage i (\ j -> wtf j)
}
