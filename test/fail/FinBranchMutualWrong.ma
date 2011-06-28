-- 2010-08-30

data Nat : Set
{ zero : Nat
; suc  : Nat -> Nat
}

data Unit : Set { unit : Unit }

-- fake product, is fun space
data Prod -(A : Set) ++(B : Set) : Set
{ pair : (A -> B) -> Prod A B
}

mutual {

  data Tree : Set
  { node : (numBranches : Nat) -> VecTree numBranches -> Tree
  }

  fun VecTree : Nat -> Set
  { VecTree zero    = Unit
  ; VecTree (suc n) = Prod Tree (VecTree n)
  }

}