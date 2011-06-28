{- 2010-01-15  Andreas Abel

Non-termination from inconsistency and injectivity of data type constructors
by the use of smart case.  (Idea from Chung-Kil Hur and Hugo Herbelin.)

Since MiniAgda does not have Set1, we use Set : Set.
-}

data Eq (A : Set 1)(a : A) : A -> Set
{ refl : Eq A a a
} 

data I (F : Set -> Set) : Set {}
 
data InvI (A : Set) : Set 1
{ inv : (X : Set -> Set) -> Eq Set (I X) A -> InvI A
} 

fun invertible : (A : Set) -> InvI A {}  -- postulate 

let cantor : Set -> Set
= \ A -> case (invertible A) 
  { (inv .A X p) -> X A 
  }

-- self-application on the type level
let cIc : Set
        = cantor (I cantor)

-- type checker loops!
fail
let loopTC : cIc
= case (invertible (I cantor))
  { (inv .(I cantor) .cantor (refl .Set .(I cantor)))  -> 
   -- in the branch, cIc --> cIc --> cIc
   -- put anything here:
        Set
  }
