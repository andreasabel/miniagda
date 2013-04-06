-- 2013-04-05

data Prod (A, B : Set) { pair (a : A) (b : B) }

data D (A : Set 1)
{ c : D (Prod A A) } -- not a pattern, currenlty not accepted
