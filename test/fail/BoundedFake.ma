-- 2012-01-22

-- need to check that bounds are forced!
fun bad : [i, j : Size] -> [A : Set] -> A
{ bad i (j < i) A = bad j j A
}

let bot : [A : Set] -> A
  = bad # #
