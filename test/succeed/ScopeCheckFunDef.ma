data Bool : Set { true : Bool; false : Bool }

fun not : Bool -> Bool
{ not true = false
; not false = true
}

fun notnot : Bool -> Bool
{ notnot x = not (not x)
}

fun T : Bool -> Set
{ T true = Bool
; T false = Bool
}

fun f : (b : Bool) -> T b -> T b
{ f true  x = x
; f false x = x
}

