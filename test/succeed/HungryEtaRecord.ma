-- 2012-02-07

-- a recursive unit type
-- 2013-03-30 must be a cofun since not SN
cofun Hungry : -(i : Size) -> Set
{ Hungry i = [j < i] -> Hungry j
}

fun D : [i : Size] -> Hungry i -> Set {}

-- Don't try this at home!
-- let unique [i : Size] (x, y : Hungry i) (d : D i x) : D i y = d
-- loops! because of infinite eta-expansion performed in equality testing
-- similar to recursive record problem
