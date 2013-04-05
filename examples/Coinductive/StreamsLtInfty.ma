
cofun Stream : ++(A : Set) -> -(i : Size) -> Set
{ Stream A i = A & ([i' < i] -> Stream A i')
}

fun head : [A : Set] [i : Size] (s : Stream A i) -> A
{ head A i (a , as) = a
}
fun tail' : [A : Set] [i < #] (s : Stream A $i) -> Stream A i
{ tail' A i (a , as) = as i
}

cofun inst : [A : Set] [i : Size] (f : [j < $i] -> Stream A j) -> Stream A i
{ inst A i f  = head A 0 (f 0) , \ i' -> case (f $i') { (a , as) -> as i' } }

-- i <= #
-- i' < i, i' < #
-- $i' < $i
-- f $i' : Stream A $i'
-- snd (f $i') : [i'' < $i'] -> Stream A i''
-- snd (f $i') i' : Stream A i'

fun tail : [A : Set] [i : Size] (s : Stream A $i) -> Stream A i
{ tail A i (a , as) = inst A i as
}
