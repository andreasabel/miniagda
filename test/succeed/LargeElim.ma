-- 2010-10-16

data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

fun add : Nat -> Nat -> Nat
{ add  zero    n = n
; add (succ m) n = succ (add m n)
}

fun Sum : Nat -> Set
{ Sum zero     = Nat
; Sum (succ n) = Nat -> Sum n
}

fun sum : (n : Nat) -> Nat -> Sum n 
{ sum zero     x = x
; sum (succ n) x = \ y -> sum n (add x y)
}

let one   : Nat = succ zero
let two   : Nat = succ one
let three : Nat = succ two
let four  : Nat = succ three

eval let six : Nat = sum four three two one zero zero
