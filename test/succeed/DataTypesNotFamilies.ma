-- 2012-01-26 omitting types in data type (not family) definitions

data Bool : Set { true ; false }

data List ++(A : Set) : Set
{ nil ; cons (head : A) (tail : List A)
}

record Prod ++(A, B : Set) : Set
{ pair (fst : A) (snd : B)
} fields fst, snd

fail data Id (a : Bool) : Bool -> Set { refl }
