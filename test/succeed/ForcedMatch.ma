data Bool : Set
{ true : Bool
; false : Bool
}

data D : Bool -> Bool -> Set
{ d00 : D false false
; d01 : D false true
; d11 : D true true
}

fun f : (b : Bool) -> [D b b] -> Bool
{ f false d00 = false
; f true d11 = true
}