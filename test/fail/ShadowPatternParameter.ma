-- 2013-04-06

data Nat { zero ; suc (n : Nat) }

data D (n : Nat)
{ c (n : Nat) : D (suc n)
}
