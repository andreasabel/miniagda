data Sigma ++(A : Set) ++(B : A -> Set) : Set 
{ pair : (fst : A) -> (snd : B fst) -> Sigma A B
}

data Eq +(A : Set)(a : A) : A -> Set 
{ refl : Eq A a a }

data EQ +(A : Set) : A -> A -> Set
{ Refl : (a : A) -> EQ A a a
}