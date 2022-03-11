-- 2010-11-01

sized codata Stream ++(A : Set) : -Size -> Set
{ cons : [i : Size] -> (head : A) -> (tail : Stream A i) -> Stream A $i
}

cofun evens : [A : Set] -> [i : Size] -> Stream A (i + i) -> Stream A i
{ evens A ($i) (cons .(i + i + 1) a (cons .(i + i) b as)) =
   cons i a (evens A i as)
}
-- this should fail because we cannot match the input stream to depth 2
-- since only i is replaced by $i
