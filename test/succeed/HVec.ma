-- 2010-06-30 heterogeneous vectors
-- 2012-01-22 parameters gone from constructors

data Unit : Set { unit : Unit }

data Prod [i : Size] (A : Set i) (B : Set i) : Set i
--{ pair : A -> B -> Prod i A B
{ pair : (fst : A) -> (snd : B) -> Prod i A B
}

fun fst' : [i : Size] -> [A : Set i] -> [B : Set i] -> Prod i A B -> A
{ fst' i A B (pair a b) = a
}

data List [i : Size] (A : Set i) : Set i
{ nil  : List i A
; cons : A -> List i A -> List i A
}

-- recursive heterogeneous vectors
fun HVecR : List 1 Set -> Set
{ HVecR (nil) = Unit
; HVecR (cons A As) = Prod 0 A (HVecR As)
}

-- inductive heterogeneous vectors
data HVec : List 1 Set -> Set 1
{ vnil  : HVec (nil)
; vcons : [A : Set] -> [As : List 1 Set] ->
          A -> HVec As -> HVec (cons A As)
}
