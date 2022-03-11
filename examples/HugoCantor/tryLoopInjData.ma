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

data Or (A, B : Set 1) : Set 1
{ inl : A -> Or A B
; inr : B -> Or A B
}

data Eq (A : Set 1)(a : A) : A -> Set
{ refl : Eq A a a
}

{-
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
-}

data I (F : Set -> Set) : Set {}

{- no needed, injectivity of data type I is used implicitely in the matching againgst refl
fun injI : (F : Set -> Set) -> (G : Set -> Set) -> Eq Set (I F) (I G) -> Eq (Set -> Set) F G
{ injI F .F (refl .Set .(I F)) = refl (Set -> Set) F
}
-}

data InvI (A : Set) : Set 1
{ inv : (X : Set -> Set) -> Eq Set (I X) A -> InvI A
}

let EM : Set 2
       = (A : Set 1) -> Or A (A -> Empty)

fun em : EM {}  -- postulate excluded middle

fun cantor : Set -> Set
{ cantor A = case (em (InvI A))
  { (inl (inv X p)) -> X A -> Empty
  ; (inr bla) -> Unit
  }
}

-- type checker loops!

let yes : cantor (I cantor)
= case (em (InvI (I cantor)))
  { (inl (inv .cantor refl) -> \ f -> f f
  ; (inr g) -> unit
  }

let no : cantor (I cantor) -> Empty
= \ f -> case (em (InvI (I cantor)))
  { (inl (inv .cantor refl) -> f f
  ; (inr g) -> g (inv cantor refl)
  }

eval let omega : Empty
          = no yes
