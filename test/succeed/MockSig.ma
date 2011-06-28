-- 2010-06-19

data MockSig ++(A : Set) ++(B : .A -> Set) : Set
{ pair : (fst : A) -> (snd : B fst) -> MockSig A B
}