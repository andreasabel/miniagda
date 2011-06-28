-- 2010-07-01  Following a question of Nisse, this example explains
--   the need for continuity check.

data Unit : Set 
{ unit : Unit 
}

sized codata Stream +(A : Set) : Size -> Set
{ cons : [i : Size] -> (head : A) -> (tail : Stream A i) -> Stream A $i
}
fields head, tail

cofun bad : [i : Size] -> [A : Set] -> (Stream A i -> Stream A i) -> Stream A i
{ bad ($ i) A f = f (cons A i (bad i A f))
}

let undef : Stream Unit # = bad # Unit (tail Unit #)

eval let diverge : Unit = head Unit # undef 

