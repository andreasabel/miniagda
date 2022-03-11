-- colist.ma -- MiniAgda colist library

cofun CoList : ++(A : Set) -> -(i : Size) -> Set
{ CoList A i = Maybe (A & ([i' < i] -> CoList A i'))
}

fun colist : [A : Set] -> [i : Size] -> |i| -> List A i -> CoList A i
{ colist A i nil               = nil
; colist A i (cons a (i', as)) = cons a (\ i'' -> colist A (max i' i'') as)
}

fun cotake : [A : Set] -> [i : Size] -> Nat i -> CoList A i -> List A i
{ cotake A i zero          as         = nil
; cotake A i n             nil        = nil
; cotake A i (suc (i', n)) (cons a l) = cons a (i', cotake A i' n (l i'))
}

fun codrop : [A : Set ] ->
             [i : Size] -> Nat i ->
             [j : Size] -> CoList A (j+i) -> CoList A j
{ codrop A i zero          j l          = l
; codrop A i n             j nil        = nil
; codrop A i (suc (i', n)) j (cons a l) = codrop A i' n j (l (j+i'))
}

-- direct encoding of tail
check
fun cotail : [A : Set] [i : Size] (l : CoList A $i) -> CoList A i
{ cotail A i nil        = nil
; cotail A i (cons a l) = l i
}

-- tail as instance of drop
let cotail [A : Set] : [i : Size] (l : CoList A $i) -> CoList A i
  = codrop A $0 (suc (0, zero))

fun coappend : [A : Set] -> [i : Size] -> |i| ->
               CoList A i -> CoList A i -> CoList A i
{ coappend A i nil        bs = bs
; coappend A i (cons a l) bs = cons a (\ i' -> coappend A i' (l i') bs)
}

-- list take

let take [A : Set] [i : Size] (n : Nat i) (as : List A i) : List A i
  = cotake A i n (colist A i as)

{-

fun cotail : [A : Set] -> [i : Size] -> CoList A $i -> CoList A i
{ cotail A i l = case l
   { nil -> nil
   ; (cons a as) -> cons a (\ j -> as $j

mapMaybe (A & ([i' < $i] -> CoList A i'))
                        (A & ([i' < i] -> CoList A i'))

}
-}
