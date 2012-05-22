-- 2012-02-07

-- a recursive unit type
record Hungry -(i : Size) : Set
{ inn (out : [j < i] -> Hungry j)
} fields out

fun D : [i : Size] -> Hungry i -> Set {}

let unique [i : Size] (x, y : Hungry i) (d : D i x) : D i y
  = d
-- loops! because of infinite eta-expansion performed in equality testing
-- similar to recursive record problem
