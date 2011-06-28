data Subset (A : Set) (P : A -> Set) : Set
{
  put : (a : A) -> [P a] -> Subset A P 
}

data PFun (A : Set)(B : Set) : Set
{ mkPFun : (dom : A -> Set) -> (app : Subset A dom -> B) -> PFun A B
}
-- should fail unless Set : Set
