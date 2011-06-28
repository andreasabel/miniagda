-- 2010-11-01 

sized codata Stream ++(A : Set) : -Size -> Set 
{ cons : [i : Size] -> (head : A) -> (tail : Stream A i) -> Stream A $i
}

-- suggested by Florent Balestrini
cofun evens : [A : Set] -> [i : Size] -> Stream A (i + i) -> Stream A i
{ evens A ($i) (cons .A .(i + i + 1) a (cons .A .(i + i) b as)) =
   cons A i a (evens A i as)
}

cofun map2 : [A, B : Set] -> (A -> B) -> 
             [i : Size] -> Stream A (2 * i) -> Stream B (2 * i)
{ map2 A B f ($ i) (cons .A .$(2 * i) a1 (cons .A .(2 * i) a2 as)) =
    cons B $(2 * i) (f a1) (cons B (2 * i) (f a2) (map2 A B f i as))
}