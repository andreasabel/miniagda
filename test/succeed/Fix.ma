-- 2012-01-27  fix-point principle

fun fix : [A : Size -> Set] -> 
  (f : [i : Size] -> ([j < i] -> A j) -> A i) ->
  [i : Size] -> |i| -> A i 
{ fix A f i = f i (fix A f)
}
