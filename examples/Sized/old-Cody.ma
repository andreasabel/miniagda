data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

sized data O : Size -> Set
{ Z : [i : Size] -> O ($ i)
; S : [i : Size] -> O i -> O ($ i)
; L : [i : Size] -> (Nat -> O i) -> O ($ i)
; M : [i : Size] -> O i -> O i -> O ($ i)
}

fun emb : Nat -> O #
{ emb zero = Z #
; emb (succ n) = S # (emb n)
}

fun pre : [i : Size] -> (Nat -> O ($ ($ i))) -> Nat -> O ($ i)
{ pre i f n = case (f (succ n))
  { (Z .($ i))   -> Z i
  ; (S .($ i) x) -> x
  ; (L .($ i) g) -> g n
  ; (M .($ i) a b) -> a
  }
}

fun mean : [i : Size] -> O i -> Nat -> Nat
{ mean .($ ($ ($ i))) (M .($ ($ i))
                             (L .($ i) f)
                             (S .($ i) (S i x)))
                          n
  = mean _ (M _ (L _ (pre _ f)) (f n)) (succ (succ (succ n)))
; mean i x n = n
}

let four : Nat
  = succ (succ (succ (succ zero)))

--eval
let bla : Nat = mean # (M # (L # emb) (emb four)) four
