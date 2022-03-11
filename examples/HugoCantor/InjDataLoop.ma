{- 2010-01-15

Non-termination from inconsistency and injectivity of data type constructors
by the use of smart case.

Since MiniAgda does not have Set1, this proof uses Set : Set.
-}

data Empty : Set {}

data Eq (A : Set 1)(a : A) : A -> Set
{ refl : Eq A a a
}

data I (F : Set -> Set) : Set {}

data InvI (A : Set) : Set 1
{ inv : (X : Set -> Set) -> Eq Set (I X) A -> InvI A
}

fun invertible : (A : Set) -> InvI A {}  -- postulate

-- self-application on the type level
let cantor : Set -> Set
= \ A -> case (invertible A)
  { (inv X p) -> X A -> Empty
  }

let cIc : Set
        = cantor (I cantor)

-- type checker loops!
-- 2010-09-23 no longer, changed case equality

let delta : cIc
= case (invertible (I cantor))
  { (inv .cantor refl)  ->
   -- in the branch, cIc --> cIc -> Empty --> (cIc -> Empty) -> Empty -->...
        \ f -> f f
  }


let delta' : cIc -> Empty
= case (invertible (I cantor))
  { (inv .cantor refl) ->
        \ f ->  f f
  }

let omega : Empty
          = delta' delta
