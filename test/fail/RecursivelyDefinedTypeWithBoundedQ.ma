-- 2011-03-23

fun T : -(i : Size) -> |i| -> Set
{ T i = [j < i] -> T j
}