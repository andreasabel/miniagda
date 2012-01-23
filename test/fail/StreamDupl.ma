-- 2010-11-01 

sized codata Stream ++(A : Set) : -Size -> Set 
{ cons : [i : Size] -> (head : A) -> (tail : Stream A i) -> Stream A $i
}
 
cofun evens : [A : Set] -> [i, j : Size] -> Stream A (i + j) -> Stream A i
{ evens A ($i) j (cons .(i + j + 1) a (cons .(i + j) b as)) =
   cons i a (evens A i as)
}