{- 2010-09-17 from Bruno Barras Coq* homepage

Section NoImplicitProductDomain.
-}

data True  : Set { trivial : True }
data False : Set {}

{-
(* The first argument of product cannot be implicit *)

Variable Pi : forall [A:Type], (A -> Type) -> Type.
Variable Lam : forall [A] [B], (forall (x:A), B x) -> Pi A B.
Variable App : forall [A] [B], Pi A B -> forall (x:A), B x.
-}

{-
fun Pi  : [i : Size] -> [A : Set i] -> (A -> Set i) -> Set i
{}
fun Lam : [i : Size] -> [A : Set i] -> [B : A -> Set i] ->
          ((x : A) -> B x) -> Pi i A B
{}
fun App : [i : Size] -> [A : Set i] -> [B : A -> Set i] ->
          Pi i A B -> (x : A) -> B x
{}
-}

-- this declaration fails, A cannot appear in constructor if its marked erased

fail data Pi [A : Set](B : A -> Set) : Set
{ Lam : (App : (x : A) -> B x) -> Pi A B
}

-- but let's assume we could have it

fun Pi  : [A : Set] -> (A -> Set) -> Set
{}
fun Lam : [A : Set] -> [B : A -> Set] ->
          ((x : A) -> B x) -> Pi A B
{}
fun App : [A : Set] -> [B : A -> Set] ->
          Pi A B -> (x : A) -> B x
{}

{-
(* F is a predicate False on domain A (implicit) *)
Definition F [A] := Pi A (fun _ => False).
-}

let F : [A : Set] -> Set 1
      = \ A -> Pi A (\ z -> False)

{-
Lemma Ftrue : F False.
red.
apply Lam.
trivial.
Qed.
-}

let Ftrue : F False
  = Lam False (\ z -> False) (\ x -> x)

{-
Lemma Ffalse : F True -> False.
unfold F; intro.
apply (App _ _ X I).
Qed.
-}


let Ffalse : F True -> False
  = \ f -> App (True) (\ z -> False) f trivial

{-
(* ... but F True and F False are convertible *)

Lemma noImplicitProductDomain : False.
Proof Ffalse Ftrue.

End NoImplicitProductDomain.
-}

let boo : False = Ffalse Ftrue

{-
(* MUST FAIL:
Inductive E [A:Type] (x:A) : A -> Prop := e : E A x x.
(equality on an implicit type: invalid in the model, but inconsistent ?)
Inductive F [A:Type] (P:A->Prop) : Prop := f : (forall (x:A), P x) -> F A P.
(F would imply the above paradox)
*)
-}

-- MiniAgda allows "E"

data Id [A : Set](a : A) : A -> Set
{ refl : Id A a a
}
