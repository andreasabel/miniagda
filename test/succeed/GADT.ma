-- 2010-10-03

data Bool : Set
{ true : Bool
; false : Bool
}

data Nat : Set
{ zero : Nat
; suc : Nat -> Nat
}

data Pair (A, B : Set) : Set
{ pair : A -> B -> Pair A B
}

data Exp : Set -> Set 1
{ nat  : Nat  -> Exp Nat
; bool : Bool -> Exp Bool
; tup  : (A, B : Set) -> Pair A B -> Exp (Pair A B)
}
