data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

fun plus : Nat -> Nat -> Nat {}

mutual {

  fun f : Nat -> Nat -> Nat
  { 
    f (succ n) m = f n (succ m) ; -- ADDING THIS LINE leads to success??
    f (succ (succ (succ n))) m = plus (m) (g n n)
  }

  fun g : Nat -> Nat -> Nat
  { g (succ n) m = plus (g n (succ m)) (f m n)
  }
}