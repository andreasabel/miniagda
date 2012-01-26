data Unit : Set
{ unit : Unit
} 

data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

data Sigma (l : Size) ++(A : Set l) ++(B : A -> Set l) : Set l
{ pair : (fst : A) -> (snd : B fst) -> Sigma l A B
} fields fst, snd

{-
data Exists (l, i : Size)(P : Size -> Set l) : Set l
{ exists : (j : Size) -> |j| < |i| -> P j -> Exists i P
}
-}

data Codes : Set
{ nat : Codes
; pi  : Codes
; set : Codes
}

{- BUG internal error: no name for variable 2
fun UEl : Nat -> (i : Size) -> Sigma 1 Set (\ U -> U -> Set)
{ UEl n i = pair -- 2 (Set 1) (\ U -> Set 1)
  (Sigma 0 Codes (\ c -> case c 
    { nat -> Unit
    ; pi  -> Unit 
    } )) 
  (\ t -> case t 
    { (pair -- .0 .Codes .(\ c -> case c { nat -> Unit ; pi  -> Unit}) 
         nat u) -> Nat  
    })
}
-}