data Empty : Set {}

data Unit : Set
{ unit : Unit
}

data Or (A : Set) (B : Set) : Set
{ inl : A -> Or A B
; inr : B -> Or A B
}

data Eq (A : Set 1)(x : A) : A -> Set
{ refl : Eq A x x
} 

fun cong : (F : Set -> Set) -> (G : Set -> Set) -> (A : Set) ->
  Eq (Set -> Set) F G -> Eq Set (F A) (G A)
{ cong F .F A (refl .(Set -> Set) .F) = refl Set (F A)
} 

fun castLR : (A : Set) -> (B : Set) -> Eq Set A B -> A -> B
{ castLR A .A (refl .Set .A) a = a
}

fun castRL : (A : Set) -> (B : Set) -> Eq Set A B -> B -> A
{ castRL A .A (refl .Set .A) a = a
}

data I (F : Set -> Set) : Set {}

fun injI : (F : Set -> Set) -> (G : Set -> Set) -> Eq Set (I F) (I G) -> Eq (Set -> Set) F G
{ injI F .F (refl .Set .(I F)) = refl (Set -> Set) F
}
 
data InvI (A : Set) : Set 1
{ inv : (X : Set -> Set) -> Eq Set (I X) A -> InvI A
} 

let EM : Set 1
       = (A : Set) -> Or A (A -> Empty)

{-
fun cantor : EM -> Set -> Set
{ cantor em A = case (em (InvI A)) 
  { (inl .(InvI (I X)) .(InvI (I X) -> Empty) (inv .(I X) X (refl .Set .(I X)))) -> 
      X (I X) -> Empty
  ; (inr .(InvI A) .(InvI A -> Empty) bla) -> Unit
  }
} 
-}

fun cantor : EM -> Set -> Set
{ cantor em A = case (em (InvI A)) 
  { (inl .(InvI A) .(InvI A -> Empty) (inv .A X p)) -> X A -> Empty
  ; (inr .(InvI A) .(InvI A -> Empty) bla) -> Unit
  }
} 


fun no : (em : EM) -> cantor em (I (cantor em)) -> Empty
{ no em f  = case (em (InvI (I (cantor em))))
  { (inl .(InvI (I (cantor em))) .(InvI (I (cantor em)) -> Empty) 
      (inv .(I (cantor em)) X p)) -> 
        f (castRL (X (I (cantor em))) 
                  (cantor em (I (cantor em))) 
            (cong X (cantor em) (I (cantor em))
              (injI X (cantor em) 
                 p)) f)
--  ; (inr .(InvI (I (cantor em))) .(InvI (I (cantor em)) -> Empty)   
  }
}

{-
fun no : (em : EM) -> cantor em (I (cantor em)) -> Empty
{ no em f  = case (em (InvI (I (cantor em))))
  { (inl .(InvI (I (cantor em))) .(InvI (I (cantor em)) -> Empty) 
      (inv .(I (cantor em)) .(cantor em) (refl .Set .(I (cantor em))))) ->
        f em f
--  ; (inr .(InvI (I (cantor em))) .(InvI (I (cantor em)) -> Empty)   
  }
}
-}
