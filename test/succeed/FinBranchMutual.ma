-- 2010-08-28

data Nat : Set
{ zero : Nat
; suc  : Nat -> Nat
}

data Unit : Set { unit : Unit }

data Prod ++(A, B : Set) : Set
{ pair : (fst : A) -> (snd : B) -> Prod A B
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
