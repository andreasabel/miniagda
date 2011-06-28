-- 2010-11-18
-- Agda eta issue 365

data Nat : Set 
{ zero : Nat
; succ : Nat -> Nat
}

data Fin : Nat -> Set
{ fzero : [n : Nat] -> Fin (succ n)
; fsucc : [n : Nat] -> Fin n -> Fin (succ n)
}

data Vec (A : Set) : Nat -> Set
{ nil  : Vec A zero
; cons : [n : Nat] -> A -> Vec A n -> Vec A (succ n)
}

data Eq (A : Set)(a : A) : A -> Set
{ refl : Eq A a a
}

fun eqCons : [A : Set] -> [n : Nat] -> [x : A] ->
  [xs, xs' : Vec A n] ->
  Eq (Vec A n) xs xs' -> 
  Eq (Vec A (succ n)) (cons A n x xs) (cons A n x xs')
{ eqCons A n x xs .xs (refl .(Vec A n) .xs) = 
    refl (Vec A (succ n)) (cons A n x xs)
}


fun comp : [A : Set] -> [B : A -> Set] -> [C : (x : A) -> B x -> Set] ->
     (f : [x : A] -> (y : B x) -> C x y) ->
     (g : (x : A) -> B x) ->
     (x : A) -> 
     C x (g x)
{ comp A B C f g x = f x (g x)
}
-- in MiniAgda, this behaves like
-- comp A B C f g = \ x -> f x (g x)

fun lookup : [n : Nat] -> [A : Set] -> Vec A n -> Fin n -> A
{ lookup .zero    A (nil .A) ()
; lookup .(succ n) A (cons .A n x xs) (fzero .n) = x
; lookup .(succ n) A (cons .A n x xs) (fsucc .n i) = lookup n A xs i
}

fun tabulate : (n : Nat) -> [A : Set] -> (Fin n -> A) -> Vec A n
{ tabulate zero     A f = nil A
; tabulate (succ n) A f = cons A n (f (fzero n)) 
   (tabulate n A (comp (Fin n) 
                       (\ i -> Fin (succ n)) 
                       (\ i j -> A) 
                       (\ i -> f) 
                       (fsucc n)))
}

fun lemma : [A : Set] -> (n : Nat) -> (xs : Vec A n) -> 
   Eq (Vec A n) (tabulate n A (lookup n A xs)) xs 
{ lemma A zero (nil .A) = refl (Vec A zero) (nil A)
; lemma A (succ n) (cons .A .n x xs) =
    eqCons A n x (tabulate n A (lookup n A xs)) xs (lemma A n xs)
}
