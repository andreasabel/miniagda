-- 2013-04-06
data Nat { zero ; suc (n : Nat) }

data Sg (n : Nat)
{ sg (n : Nat) : Sg n
}
-- this should be illegal shadowing, because it is confusing
-- (even the type checker gets confused)
