-- 2009-06-25 eta expansion for the unit type

data Unit : Set
{
  unit : Unit
}

fun P : Unit -> Set
{
  P unit = Unit
}

fun p : (u : Unit) -> P u
{
  p x = unit
}

fun q : (u : Unit) -> P u
{
  q unit = unit
}

-- what also should work is
-- q .unit = unit

-- 2009-09-19

data Bool : Set
{ true  : Bool
; false : Bool
}

let r' : Bool -> Unit
       = \ b -> unit

let pr' : (b : Bool) -> P (r' b)
       = \ b -> unit

fun r : Bool -> Unit
{ r true = unit
; r false = unit
}

-- definitions need also to be eta-expanded
-- otherwise the following does not typecheck
let pr : (b : Bool) -> P (r b)
       = \ b -> unit
