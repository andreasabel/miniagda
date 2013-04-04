-- 2013-04-04 This should not termination check:
fun f : [i : Size] |i| [j < i] -> |i| < |j| -> Set
{ f i j = f j i }
