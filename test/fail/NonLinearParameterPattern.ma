data Bool { false ; true }

data D (x, y : Bool)
{ c : D x x }

fun g : D true true -> Set
{ g c = Bool }

fail
fun f : D true false -> Set
{ f c = Bool }
-- should not match!

let v : D true false = c
-- should also fail
