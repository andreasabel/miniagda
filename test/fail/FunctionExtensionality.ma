-- 2012-03-07 chat with Nicolai Kraus

data Id (A : Set) (a : A) : A -> Set
{ refl : Id A a a
}
check
fun subst : [A : Set] [a, b : A] [q : Id A a b]
            [P : A -> Set] -> P a -> P b
{ subst A a .a refl P h = h
}

fun J : [A : Set] [P : (a,b : A) -> Id A a b -> Set]
  (h : (a : A) -> P a a refl) (a,b : A) [q : Id A a b] -> P a b q
{ J A P h a .a refl = h a
}

-- defining subst from J
let subst [A : Set] (a, b : A) (q : Id A a b)
          [P : A -> Set] : P a -> P b
  = J A (\ x y p -> P x -> P y) (\ y p -> p) a b q

-- extensionality axiom
fun ext : [A : Set] [B : A -> Set] [f, g : (x : A) -> B x]
  (h : [x : A] -> Id (B x) (f x) (g x)) ->
  Id ((x : A) -> B x) f g {}

let extReducesNot [A : Set] [a : A] [f : A -> A] [p : [x : A] -> Id A x (f x)] :
  Id A a (subst (A -> A) (\ x -> x) f
           (ext A (\ x -> A) (\ x -> x) f p)
           (\ x -> A)
           a)
  = refl
