-- 2010-09-14 Dan Doel on the coq list

data T (i : Size) : Set $i
{ inn : (out : Set i) -> T i
}

fun U : (i : Size) -> T $i
{ U i = inn (T i)
}
