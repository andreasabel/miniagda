-- 2012-01-25

-- record
data Sigma ++(A : Set) ++(B : A -> Set) : Set
{ pair (fst : A) (snd : B fst) : Sigma A B
} fields fst, snd

fun eta : [A, B : Set] -> Sigma A (\ x -> B) -> Sigma A (\ x -> B)
{ eta A B p = pair (fst p) (snd p)
}

let builtinEta [A, B : Set] (p : Sigma A (\ x -> B)) 
  : < pair (fst p) (snd p) : Sigma A (\ x -> B) >
  = p