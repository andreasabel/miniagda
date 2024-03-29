-- 2010-07-09 Workshop on Dependently Typed Programming DTP-10

data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

-- Erasing index arguments from constructors -------------------------

data Vec ++(A : Set) : Nat -> Set
{ vnil  : Vec A zero
; vcons : [n : Nat] -> (vhead : A) -> (vtail : Vec A n) -> Vec A (succ n)
}
fields vhead, vtail

-- ok, but boring
fun length0 : [A : Set] -> (n : Nat) -> Vec A n -> <n : Nat>
{ length0 A n v = n
}

-- cannot use irrelevant function argument [n : Nat]
fail fun length1 : [A : Set] -> [n : Nat] -> Vec A n -> <n : Nat>
{ length1 A n v = n
}

-- cannot use irrelevant constructor argument
fail fun length2 : [A : Set] -> [n : Nat] -> Vec A n -> <n : Nat>
{ length2 A .zero vnil = zero
; length2 A .(succ n) (vcons n a v) = succ n
}

{-  -- 2012-01-25 fix for broken singleton constructor types no longer neede
let succ_ [n : Nat] : <n : Nat> -> <succ n : Nat> = \ m -> succ m

-- recursive solution, extracts to  length : List A -> Nat
fun length : [A : Set] -> [n : Nat] -> Vec A n -> <n : Nat>
{ length A .zero vnil = zero
; length A .(succ n) (vcons n a v) = succ_ n (length A n v)
}
-}

-- recursive solution, extracts to  length : List A -> Nat
fun length : [A : Set] -> [n : Nat] -> Vec A n -> <n : Nat>
{ length A .zero vnil = zero
; length A .(succ n) (vcons n a v) = succ (length A n v)
}

-- Proof irrelevance in data types -----------------------------------

data Leq (n : Nat) : Nat -> Set
{ leqEq : Leq n n
; leqS  : [m : Nat] -> Leq n m -> Leq n (succ m)
}

data SList (n : Nat) : Set
{ snil  : SList n
; scons : (m : Nat) -> [Leq m n] -> SList m -> SList n
}

data Id [A : Set](a : A) : A -> Set
{ refl : Id A a a
}

fun prfIrrCons : [n, m : Nat] -> [p1, p2 : Leq m n] -> [l : SList m] ->
                 Id (SList n) (scons m p1 l) (scons m p2 l)
{ prfIrrCons n m p1 p2 l = refl -- (SList n) (scons n m p1 l)
}

-- Existentials ------------------------------------------------------

data Exists (A : Set)(P : A -> Set) : Set
{ exIntro : [a : A] -> P a -> Exists A P
}

-- Large existentials
impredicative data EXISTS [i : Size](A : Set i)(P : A -> Set) : Set
{ eXIntro : [a : A] -> P a -> EXISTS i A P
}

-- projections not definable (weak Sigma)
fail fun proj1 : [i : Size] -> [A : Set i] -> [P : A -> Set] ->
                 EXISTS i A P -> A
{ proj1 i A P (eXIntro a p) = a -- a cannot appear here!
}

-- Exists elimination
fun eXElim : [i : Size] -> [A : Set i] -> [P : A -> Set] ->
             EXISTS i A P -> [C : Set] -> ([a : A] -> P a -> C) -> C
{ eXElim i A P (eXIntro a p) C k = k a p
}

-- Subsets -----------------------------------------------------------

data Subset (A : Set) (P : A -> Set) : Set
{ inSub : (outSub : A) -> [P outSub] -> Subset A P
}
fields outSub

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

-- trustme -- because of deep matching bug in MiniAgda
fun joinPrf : [A : Set] -> Prf (Prf A) -> Prf A
{ joinPrf A (prf (prf a)) = prf a
}

fail fun bindPrf : [A, B : Set] -> Prf A -> (A -> Prf B) -> Prf B
{ bindPrf A B (prf a) f = f a  -- a cannot be used here
}

let bindPrf : [A, B : Set] -> Prf A -> (A -> Prf B) -> Prf B
  = \ A B pa f -> joinPrf B (mapPrf A (Prf B) f pa)

-- More laws for bracket types

-- does not go this way
fail fun isoForall1 : [A : Set] -> [B : A -> Set] ->
                 ((x : A) -> Prf (B x)) -> Prf ((x : A) -> B x)
{ isoForall1 A B f = prf (\ x -> f x)
}

fun isoForall2 : [A : Set] -> [B : A -> Set] ->
                 Prf ((x : A) -> B x) -> (x : A) -> Prf (B x)
{ isoForall2 A B (prf f) x = prf (f x)
}


data Prod ++(A, B : Set) : Set
{ pair : (fst : A) -> (snd : B) -> Prod A B
}
fields fst, snd

fun isoAnd1 : [A, B : Set] -> Prod (Prf A) (Prf B) -> Prf (Prod A B)
{ isoAnd1 A B (pair (prf a) (prf b)) = prf (pair a b)
}

fun isoAnd2 : [A, B : Set] -> Prf (Prod A B) -> Prod (Prf A) (Prf B)
{ isoAnd2 A B (prf (pair a b)) = pair (prf a) (prf b)
}

