-- 2009-11-28
-- illegal use of size index (destroys monotonicity)

sized codata Stream (A : Set) : Size -> Set
{ consStream : (i : Size) -> A -> Stream A i -> Stream A ($ i)
}

sized data NotMon (A : Set) : Size -> Set
{ consBla : (i : Size) -> Stream A i -> NotMon A i -> NotMon A ($ i)
} 