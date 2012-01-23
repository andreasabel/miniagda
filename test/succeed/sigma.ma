data Sigma ++(A : Set) ++(B : A -> Set) : Set
{ pair : (fst : A) -> (snd : B fst) -> Sigma A B
}
fields fst, snd

-- Destructors generated:
--
-- fun fst : [A : Set] -> [B : A -> Set] -> (p : Sigma A B) -> A
-- { fst A B (pair .A .B a b) = a }
-- fun snd : [A : Set] -> [B : A -> Set] -> (p : Sigma A B) -> B (fst A B p)
-- { snd A B (pair .A .B a b) = b }

-- infinite trees
codata IT : Set
{ it : Sigma IT (\ x -> IT) -> IT
}


data Id ++(A : Set)(a : A) : A -> Set
{ refl : Id A a a
}

-- eta equality for neutral terms
let etaSigma : (A : Set) -> (B : A -> Set) -> (p : Sigma A B) -> 
               Id (Sigma A B) p (pair (fst A B p) (snd A B p))
             = \ A -> \ B -> \ p -> refl -- (Sigma A B) p
data Bool : Set
{ true : Bool
; false : Bool
}

let Bool2 : Set 
          =  Sigma Bool (\ b -> Bool)
let pair2 : Bool -> Bool -> Bool2
          = \ b1 b2 -> pair {- Bool (\ b -> Bool)-} b1 b2
let fst2  : Bool2 -> Bool
          = fst Bool (\ b -> Bool)
let snd2  : Bool2 -> Bool
          = snd Bool (\ b -> Bool)

fun bla : Bool -> Bool2
{ bla true  = pair2 true false
; bla false = pair2 false false
}


-- eta equality for arbitrary terms
let etaBool2 : (b : Bool) -> Id Bool2 (bla b) (pair2 (fst2 (bla b)) (snd2 (bla b)))
             = \ b -> refl -- Bool2 (bla b)
