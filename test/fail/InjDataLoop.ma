{- 2010-01-15

Non-termination from inconsistency and injectivity of data type constructors
by the use of smart case.

2010-06-25 Switching to predicative polymorphism
-}

data Empty : Set {}

data Eq [i : Size](A : Set i)(a : A) : A -> Set
{ refl : Eq i A a a
}

data I (F : Set -> Set) : Set {}

data InvI (A : Set) : Set 1
{ inv : (Inverse : Set -> Set) -> Eq 1 Set (I Inverse) A -> InvI A
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
let delta : cIc
= case (invertible (I cantor))
  { (inv {-.(I cantor)-} .cantor (refl {-.1 .Set .(I cantor)-}))  ->
   -- in the branch, cIc --> cIc -> Empty --> (cIc -> Empty) -> Empty -->...
        \ f -> f f
  }

let delta' : cIc -> Empty
= case (invertible (I cantor))
  { (inv {-.(I cantor)-} .cantor (refl {-.Set .(I cantor)-})) ->
        \ f ->  f f
  }

let omega : Empty
          = delta' delta
