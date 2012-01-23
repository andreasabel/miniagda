-- 2011-03-16

data Nat : Set
{ zero : Nat
; suc  : Nat -> Nat
}

data List (A : Set) : Nat -> Set 
{ nil  : List A zero
; cons : [n : Nat] -> (head : A) -> (tail : List A n) -> List A (suc n)
} fields head, tail

data Add : Nat -> Nat -> Nat -> Set
{ addZ : [x : Nat] -> Add zero x x
; addS : [x,y,z : Nat] -> (addP : Add x y z) -> Add (suc x) y (suc z)
} fields addP

fun append : [A : Set] -> [n,m,l : Nat] -> List A n -> List A m ->
  [Add n m l] -> List A l
{ append A .zero    m .m       (nil)         ys (addZ .m) = ys
; append A .(suc n) m .(suc l) (cons n x xs) ys (addS .n .m l p) =
   cons l x (append A n m l xs ys p)
}  

fun bla : [Nat] -> ([Nat] -> Nat) -> Nat
{ bla x k = k x
}