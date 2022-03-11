-- 2010-01-30 (JFLA 2010, explaining Cody)

data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

let one : Nat = succ zero
let two : Nat = succ one

sized data List (A : Set) : Size -> Set
{ nil  : (i : Size) -> List A ($ i)
; cons : (i : Size) -> A -> List A i -> List A ($ i)
}

let alist : List Nat # = (cons # one (cons # zero (nil #)))

fun map : (A : Set) -> (B : Set) -> (i : Size) -> (A -> B) -> List A i -> List B i
{ map A B .($ i) f (cons i a l) = cons i (f a) (map A B i f l)
; map A B .($ i) f (nil i)      = nil i
}

let alist' : List Nat # = map Nat Nat # (\ x -> x) alist

{- illegal

fun bla : (i : Size) -> Nat
{ bla ($ i) = bla i
}

let blub : Nat = bla #
-}
