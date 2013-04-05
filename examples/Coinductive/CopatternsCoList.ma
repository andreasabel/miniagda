data CoListF ++(A : Set) ++(X : Set) 
{ nil 
; cons (head : A) (tail : X)
}

record CoList ++(A : Set) -(i : Size) : Set
{ inn (out : [j < i] -> CoListF A (CoList A j))
} fields out

fun repeat : [A : Set] -> (a : A) -> [i : Size] -> CoList A i
{ repeat A a i .out j = cons a (repeat A a j)
}

fun map : [A, B : Set] -> (f : A -> B) -> [i : Size] -> CoList A i -> CoList B i
{ map A B f i l .out j = case l .out j
  { nil -> nil
  ; (cons a as) -> cons (f a) (map A B f j as)
  }
}