-- 2010-07-09 Workshop on Dependently Typed Programming DTP-10
-- 2010-09-21 Email discussion on Agda list with Dan Doel
-- 2012-01-22 parameters gone from constructors

data Id [A : Set](a : A) : A -> Set
{ refl : Id A a a
}

fun elimId : [A : Set] -> [P : A -> Set] -> [a, b : A] -> [Id A a b] ->
             P a -> P b
{ elimId A P a .a refl h = h
}

-- Existentials ------------------------------------------------------

data Ex (A : Set)(P : A -> Set) : Set
{ exIntro : [a : A] -> P a -> Ex A P
}

-- Large existentials 
impredicative data Exists [i : Size](A : Set i)(P : A -> Set) : Set
{ ExIntro : [a : A] -> P a -> Exists i A P
}

-- projections not definable (weak Sigma)
fail fun proj1 : [i : Size] -> [A : Set i] -> [P : A -> Set] -> 
                 Exists i A P -> A
{ proj1 i A P (ExIntro a p) = a -- a cannot appear here!
}

-- Exists elimination
fun ExElim : [i : Size] -> [A : Set i] -> [P : A -> Set] -> 
             Exists i A P -> [C : Set] -> ([a : A] -> P a -> C) -> C
{ ExElim i A P (ExIntro a p) C k = k a p
}

-- Subsets -----------------------------------------------------------

data Subset (A : Set) (P : A -> Set) : Set
{ inSub : (outSub : A) -> [P outSub] -> Subset A P
}

fun outSub' : [A : Set] -> [P : A -> Set] -> Subset A P -> A
{ outSub' A P (inSub a p) = a
}

-- Proof-irrelevant propositions (Proof types / bracket types) -------

data Prf ++(A : Set) : Set
{ prf : [A] -> Prf A
}

fun proofIrr : [A : Set] -> [a, b : Prf A] -> Id (Prf A) a b
{ proofIrr A (prf a) (prf b) = refl
}

fail fun proofIrr' : [A : Set] -> [a, b : Prf A] -> Id (Prf A) a b
{ proofIrr' A a b = refl
}

-- Monad Laws for Prf

fun mapPrf : [A, B : Set] -> (A -> B) -> Prf A -> Prf B
{ mapPrf A B f (prf a) = prf (f a)
}

fun joinPrf : [A : Set] -> Prf (Prf A) -> Prf A
{ joinPrf A (prf (prf a)) = prf a
}

fail fun bindPrf : [A, B : Set] -> Prf A -> (A -> Prf B) -> Prf B
{ bindPrf A B (prf a) f = f a  -- a cannot be used here
}

let bindPrf : [A, B : Set] -> Prf A -> (A -> Prf B) -> Prf B
  = \ A B pa f -> joinPrf B (mapPrf A (Prf B) f pa)

{- Dan Doel, eliminator for "Squash" = Prf

I believe this is equivalent to what the thesis refers to as token
type target erasure. It would make the Squash eliminator:

 elimSq : (A : Set) => (P : Squash A -> Set) =>
          (f : (x : A) => P (squash x)) ->
          (s : Squash A) => P s
 elimSq A P f (squash x) = f x

and in general, it would improve the eliminator of any singleton type
in the same way. However, the problem is that equality types are in
this class, and if you make those erasable, you get bad meta-theoretic
properties. -}

fun elimPrf : [A : Set] -> [P : Prf A -> Set] ->
              (f : [a : A] -> P (prf a)) ->
              [x : Prf A] -> P x
{ elimPrf A P f (prf a) = f a 
}

-- More laws for bracket types

-- does not go this way
fail fun isoForall1 : [A : Set] -> [B : A -> Set] ->
                 ((x : A) -> Prf (B x)) -> Prf ((x : A) -> B x)
{ isoForall1 A B f = prf {-((x : A) -> B x)-} (\ x -> f x)
}

fun isoForall2 : [A : Set] -> [B : A -> Set] ->
                 Prf ((x : A) -> B x) -> (x : A) -> Prf (B x)
{ isoForall2 A B (prf {-.((x' : A) -> B x')-} f) x = prf {-(B x)-} (f x)
}


data Prod ++(A, B : Set) : Set
{ pair : (fst : A) -> (snd : B) -> Prod A B
}

fun isoAnd1 : [A, B : Set] -> Prod (Prf A) (Prf B) -> Prf (Prod A B)
{ isoAnd1 A B (pair (prf a) (prf b)) =
    prf (pair a b)
}

fun isoAnd2 : [A, B : Set] -> Prf (Prod A B) -> Prod (Prf A) (Prf B)
{ isoAnd2 A B (prf (pair a b)) = 
    pair (prf a) (prf b)
}


