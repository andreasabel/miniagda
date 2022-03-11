-- 2010-06-20
-- sized inductive types
-- 2012-01-22 parameters gone from constructors

data Empty : Set {}
data Unit  : Set { unit : Unit }
data Sum ++(A : Set) ++(B : Set) : Set
{ inl : A -> Sum A B
; inr : B -> Sum A B
}
data Prod ++(A : Set) ++(B : Set) : Set
{ pair : (fst : A) -> (snd : B) -> Prod A B
}

sized data Mu ++(F : ++Set -> Set) : +Size -> Set
{ inn : [i : Size] -> (out : F (Mu F i)) -> Mu F ($ i)
}

fun myout : [F : ++Set -> Set] -> [i : Size] -> Mu F ($ i) -> F (Mu F i)
{ myout F i (inn .i t) = t
}

-- iteration (universal property of Mu)
fun iter : [F : ++Set -> Set] ->
           (mapF : [A : Set] -> [B : Set] -> (A -> B) -> F A -> F B) ->
           [G : Set] -> (step : F G -> G) ->
           [i : Size] -> Mu F i -> G
{- iter F mapF G step .($ j) (inn .F j t) =
   step (mapF (Mu F j) G (iter F mapF G step j) t)
-}
{ iter F mapF G step i (inn (i > j) t) =
   step (mapF (Mu F j) G (iter F mapF G step j) t)
}

let NatF : ++Set -> Set         = \ X -> Sum Unit X
let Nat  : +Size -> Set         = Mu NatF

let zero : [i : Size] -> Nat ($ i)
         = \ i -> inn i (inl unit)

let succ : [i : Size] -> Nat i -> Nat ($ i)
         = \ i -> \ n -> inn i (inr n)


let ListF : ++Set -> ++Set -> Set = \ A -> \ X -> Sum Unit (Prod A X)
let List  : ++Set -> +Size -> Set = \ A -> Mu (ListF A)
