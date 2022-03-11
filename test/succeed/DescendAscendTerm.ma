data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

fun plus : Nat -> Nat -> Nat {}

mutual {

  fun f : Nat -> Nat
  { f (succ (succ (succ n))) = g n n
  }

  fun g : Nat -> Nat -> Nat
  { g (succ n) m = plus (g n (succ m)) (f n)
  }
}
