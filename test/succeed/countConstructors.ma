-- 2010-01-13

data Nat : Set 
{ zero : Nat
; succ : Nat -> Nat
}

fun plus : Nat -> Nat -> Nat {}

mutual {
  fun f1 : Nat -> Nat
  { f1 zero = zero
  ; f1 (succ zero) = zero
  ; f1 (succ (succ n)) = g1 n
  }

  fun g1 : Nat -> Nat
  { g1 zero = zero
  ; g1 (succ n) = f1 (succ (succ n))
  }
}

mutual {
  fun f : Nat -> Nat
  { f zero = zero
  ; f (succ zero) = zero
  ; f (succ (succ n)) = g n
  }

  fun g : Nat -> Nat
  { g zero = zero
  ; g (succ n) = plus (f n) (plus (f (succ n)) (f (succ (succ n))))
  }
}

