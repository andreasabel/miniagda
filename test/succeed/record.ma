-- a non-dependent record

data Pair (A : Set) (B : Set) : Set
{
  pair : (fst : A) -> (snd : B) -> Pair A B
}
fields fst, snd

fun swap : (A : Set) -> Pair A A -> Pair A A
{
  swap A p = pair (snd p) (fst p)
}

-- eta law
-- p = pair (fst p) (snd p) : Pair A B

-- a record with dependent destructors

data Sigma (A : Set) (B : A -> Set) : Set
{
  pair' : (fst' : A) -> (snd' : B fst') -> Sigma A B
}
fields fst', snd'

{- destructors

fst' : (A : Set) -> (B : A -> Set) -> (p : Sigma A B) -> A
snd' : (A : Set) -> (B : A -> Set) -> (p : Sigma A B) -> B (fst p)

-- eta law
-- p = pair' (fst' p) (snd' p) : Sigma A B

-}
