-- bintree.ma  Binary trees

cofun BinTree : ++(A : Set) -> +(i : Size) -> Set 
{ BinTree A i = let ++T = [j < i] & BinTree A j
                in  Maybe (T & A & T)
}
pattern leaf       = nothing
pattern node l a r = just (l, a, r)



