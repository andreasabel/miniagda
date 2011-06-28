data Id (A : Set) (a : A) : A -> Set 
{ refl : Id A a a 
}

fun subst : (A : Set) -> (a : A) -> (b : A) -> Id A a b -> 
  (P : A -> Set) -> P a -> P b
{ subst A a .a (refl .A .a) P x = x
}

-- this demonstrates eta expansion at the identity type
let bla :  (A : Set) -> (a : A) -> (p : Id A a a) -> 
           (P : A -> Set) -> (x : P a) -> 
              Id (P a) x (subst A a a p P x)
        =  \ A -> \ a -> \ p -> \ P -> \ x -> refl (P a) x

fun resp : (A : Set) -> (a : A) -> (b : A) -> Id A a b -> 
  (C : Set) -> (f : A -> C) -> Id C (f a) (f b)
{ resp A a .a (refl .A .a) C f = refl C (f a)
}
 
-- Needs heterogeneous equality
-- fun resp : (A : Set) -> (a : A) -> (b : A) -> Id A a b -> (P : A -> Set) -> (f : (x : A) -> P x) -> Id (P a) (f a) (f b)
-- { resp A a .a (refl .A .a) P f = refl (P a) (f a)
-- }
 
data True : Set 
{ trueI : True
}

data False : Set
{ }

let falseIrr : (p : False) -> (q : False) -> Id False p q
             = \ p  -> \ q -> refl False p

fun falseE : False -> (A : Set) -> A
{ }

data And (A : Set) (B : Set) : Set 
{ andI : (andE1 : A) -> (andE2 : B) -> And A B
}

data Forall (A : Set) (B : A -> Set) : Set
{ forallI : (forallE : (a : A) -> B a) -> Forall A B
}

fun shapeForallTrue : (A : Set) -> (p : Forall A (\ a -> True)) ->
  Id (Forall A (\ a -> True)) p (forallI A (\ a -> True) (\ a -> trueI))
{ shapeForallTrue A p = refl (Forall A (\ a -> True)) p
}

data Prop (A : Set) : Set
{ true   : Prop A
; false  : Prop A
; and    : Prop A -> Prop A -> Prop A
; forall : (A -> Prop A) -> Prop A
}
 
fun Proof : (A : Set) -> Prop A -> Set
{ Proof A (true .A) = True
; Proof A (false .A) = False
; Proof A (and .A p q) = And (Proof A p) (Proof A q)
; Proof A (forall .A h) = Forall A (\ a -> Proof A (h a))
}

fun proofIrr : (A : Set) -> (P : Prop A) -> (p : Proof A P) -> (q : Proof A P) -> Id (Proof A P) p q
{ proofIrr A (true .A) p q = refl True p 
; proofIrr A (false .A) p q = refl False p 
-- ; proofIrr A (and .A P Q) (andI .(Proof A P) .(Proof A Q) p1 p2) (andI .(Proof A P) .(Proof A Q) q1 q2) = (proofIrr A P p1 p2) (proofIrr A Q q1 q2) -- etc pp
}