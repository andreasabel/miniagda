{- 2010-01-12

Chung-Kil Hur's inconsistency from injectivity of data type constructors,
simplified by Hugo Herbelin.

Since MiniAgda does not have Set1, this proof uses Set : Set 
(which is also inconsistent, but not so straightforwardly).

MiniAgda also does not check case coverage, but this can be easily
verified by hand.
-}

data Empty : Set {}

data Unit : Set
{ unit : Unit
}

data Or (A : Set) (B : Set) : Set
{ inl : A -> Or A B
; inr : B -> Or A B
}

data Eq (A : Set 1)(a : A) : A -> Set
{ refl : Eq A a a
} 

fun cong : (F : Set -> Set) -> (G : Set -> Set) -> (A : Set) ->
  Eq (Set -> Set) F G -> Eq Set (F A) (G A)
{ cong F .F A refl = refl
} 

fun castLR : (A : Set) -> (B : Set) -> Eq Set A B -> A -> B
{ castLR A .A refl a = a
}

fun castRL : (A : Set) -> (B : Set) -> Eq Set A B -> B -> A
{ castRL A .A refl a = a
}

data I (F : Set -> Set) : Set {}

fun injI : (F : Set -> Set) -> (G : Set -> Set) -> Eq Set (I F) (I G) -> Eq (Set -> Set) F G
{ injI F .F refl = refl
}
 
data InvI (A : Set) : Set 1
{ inv : (X : Set -> Set) -> Eq Set (I X) A -> InvI A
} 

let EM : Set 1
       = (A : Set) -> Or A (A -> Empty)

fun em : EM {}  -- postulate excluded middle

fun cantor : Set -> Set
{ cantor A = case (em (InvI A)) 
  { (inl (inv X p)) -> X A -> Empty
  ; (inr bla) -> Unit
  }
} 

{- now define (modulo casts and irrelevant cases)

   no  = \ f -> f f
   yes = \ f -> f f
-}

let no : cantor (I cantor) -> Empty
= \ f -> case (em (InvI (I cantor)))
  { (inl 
      (inv X p)) -> 
        f (castRL (X (I cantor)) 
                  (cantor (I cantor)) 
                  (cong X cantor (I cantor) (injI X cantor p)) 
                  f)
  ; (inr g) -> g (inv cantor refl)
  }


let yes : cantor (I cantor)
= case (em (InvI (I cantor)))
  { (inl (inv X p)) -> 
        \ f -> (castLR (X (I cantor)) 
                  (cantor (I cantor)) 
                  (cong X cantor (I cantor) (injI X cantor p)) 
                  f) f
  ; (inr g) -> unit
  }


eval let omega : Empty
          = no yes
