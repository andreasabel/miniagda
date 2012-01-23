-- 2012-01-22 parameters gone from constructors

data Subset (A : Set) (P : A -> Set) : Set
{
  put : (get : A) -> [P get] -> Subset A P 
}

data Nat : Set
{ 
  zero : Nat;
  succ : Nat -> Nat
}

data Odd : Nat -> Set
{
  odd1  : Odd (succ zero);
  odd3  : Odd (succ (succ (succ zero)));
  oddSS : [n : Nat] -> Odd n -> Odd (succ (succ n))
}


data Eq (A : Set)(a : A) : A -> Set
{
  refl : Eq A a a
}

let OddN : Set 
         = Subset Nat Odd

let one  : Nat
         = succ zero

let three : Nat
          = succ (succ one) 

let o3   : OddN
         = put three odd3

let o3'  : OddN
         = put three (oddSS one odd1)

let p    : Eq OddN o3 o3'
         = refl 
