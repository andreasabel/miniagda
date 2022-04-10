data Id (A : Set) (a : A) : A -> Set
{ refl : Id A a a
}

fun cong : [A, B : Set] (f : A -> B) [x, y : A] (eq : Id A x y) -> Id B (f x) (f y)
{ cong A B f x .x refl = refl
}

fun subst : [A : Set] (P : A -> Set) [x, y : A] (eq : Id A x y) -> P x -> P y
{ subst A P x .x refl p = p
}

data Nat : Set
{ zero
; suc (n : Nat)
}

fun plus : (n, m : Nat) -> Nat
{ plus zero    m = m
; plus (suc n) m = suc (plus n m)
}

fun plus_zero : (n : Nat) -> Id Nat (plus n zero) n
unfold plus
{ plus_zero zero = refl
; plus_zero (suc n) = cong Nat Nat suc (plus n zero) n (plus_zero n)
}

fun plus_suc : (n, m : Nat) -> Id Nat (plus n (suc m)) (suc (plus n m))
unfold plus
{ plus_suc zero    m = refl
; plus_suc (suc n) m = cong Nat Nat suc (plus n (suc m)) (suc (plus n m)) (plus_suc n m)
}

fun plus_assoc : (n, m, l : Nat) -> Id Nat (plus (plus n m) l) (plus n (plus m l))
unfold plus
{ plus_assoc zero    m l = refl
; plus_assoc (suc n) m l = cong Nat Nat suc (plus (plus n m) l) (plus n (plus m l)) (plus_assoc n m l)
}

data Vec (A : Set) : Nat -> Set
{ nil  : Vec A zero
; cons : (m : Nat) (x : A) (xs : Vec A m) -> Vec A (suc m)
}

fun append : [A : Set] [n, m : Nat] (xs : Vec A n) (ys : Vec A m) -> Vec A (plus n m)
unfold plus
{ append A .zero    m nil           ys = ys
; append A .(suc n) m (cons n x xs) ys = cons (plus n m) x (append A n m xs ys)
}

fun append_nil : [A : Set] [n : Nat] (xs : Vec A n) ->
  Id (Vec A n) (subst Nat (Vec A) (plus n zero) n (plus_zero n) (append A n zero xs nil)) xs
-- unfold subst, append, plus_zero, plus
{ append_nil A .zero    nil           = refl  -- this should not check without unfolding subst
-- ; append_nil A .(suc n) (cons n x xs) =
--   cong
--     (Vec A n) (Vec A (suc n)) (cons n x)
--     (subst Nat (Vec A) (plus n zero) n (plus_zero n) (append A n zero xs nil)) xs
--     (append_nil A n xs)
}
