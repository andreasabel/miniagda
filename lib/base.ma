-- 2012-02-01 MiniAgda Library (not universe polymorphic)

-- Leibniz equality  (the only family)

data Id [A : Set](a : A) : A -> Set
{ refl : Id A a a
}

fun subst : [A : Set] -> [P : A -> Set] -> [a, b : A] -> Id A a b -> P a -> P b
{ subst A P a .a refl h = h
}

fun cong : [A : Set] -> [B : A -> Set] -> [f : (x : A) -> B x] -> 
  [a, b : A] -> (p : Id A a b) -> 
  Id (B b) (subst A B a b p (f a)) (f b)
{ cong A B f a .a refl = refl
}

-- Enumerations and sums

data Empty {}
data Unit { unit }

-- * Booleans

data Bool { true; false }

fun if : [A : Set] -> (b : Bool) -> (t, e : A) -> A
{ if A true  t e = t
; if A false t e = e
}

fun If : (b : Bool) -> ++(A, B : Set) -> Set
{ If true  A B = A
; If false A B = B
}

-- * Either: disjoint sum type

let Either ++(A, B : Set) = (b : Bool) & If b B A
pattern left  a = (false, a)
pattern right b = (true, b)

fun either : [A, B : Set] -> [C : Either A B -> Set] ->
  ((a : A) -> C (left a)) ->
  ((b : B) -> C (right b)) ->
  (x : Either A B) -> C x
{ either A B C l r (left  a) = l a
; either A B C l r (right b) = r b
}

fun EitherT : [A, B : Set] -> (A -> Set) -> (B -> Set) -> Either A B -> Set
{ EitherT A B l r (left  a) = l a
; EitherT A B l r (right b) = r b
}

let mapEither [A, B, A', B' : Set] (f : A -> A') (g : B -> B')
  : Either A B -> Either A' B'
  = either A B (\ x -> Either A' B') (\ a -> left (f a)) (\ b -> right (g b))

-- * Maybe: option type

let Maybe ++(A : Set) = Either Unit A
pattern nothing = left unit
pattern just a  = right a

let maybe [A, B : Set] (n : B) (j : A -> B) : Maybe A -> B
  = either Unit A (\ x -> B) (\ u -> n) j 

let mapMaybe [A, B : Set] (f : A -> B) : Maybe A -> Maybe B
  = mapEither Unit A Unit B (\ u -> u) f

-- * Trichonomy

data Three { one; two; three }

fun ThreeT : (t : Three) -> ++(A, B, C : Set) -> Set
{ ThreeT one   A B C = A
; ThreeT two   A B C = B
; ThreeT three A B C = C
}

let Tri ++(A, B, C : Set) = (t : Three) & ThreeT t A B C
pattern first  a = (one, a)
pattern second b = (two, b)
pattern third  c = (three, c)
