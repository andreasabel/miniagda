-- 2011-01-19
{- Message of ViktorVafeiadis on the Coq mailing list, 2011-01-18

Dear Coq-club,

I am stuck proving the following goal:

Variable A : Type.
Variable R : A -> A -> Prop.
Variable S : A -> Prop.

CoInductive trace: Type :=
 | Tnil
 | Tcons (x: A) (t: trace).
-}

-- "trace" is the type of colists
sized codata CoList (A : Set) : Size -> Set 
{ tnil  : [i : Size] -> CoList A $i
; tcons : [i : Size] -> A -> CoList A i -> CoList A $i
}

{-
CoInductive repet (s : A) : Prop :=
 | repet_S : S s -> repet s
 | repet_R : forall s', R s s' -> repet s' -> repet s.
-}
 
-- "repet"
sized codata Reach (A : Set) (S : A -> Set) (R : A -> A -> Set) *(a : A) : Size -> Set
{ start : [i : Size] -> S a -> Reach A S R a $i
; step  : [i : Size] -> (a' : A) -> R a a' -> Reach A S R a' i -> Reach A S R a $i
}

{-
CoInductive repet1 (s : A) : trace -> Prop :=
 | repet1_S : S s -> repet1 s Tnil
 | repet1_R : forall s' t, R s s' -> repet1 s' t -> repet1 s (Tcons s' t).

(* repet1 is just like repet, but has an extra argument remembering
  the trace of intermediate states. *)
-}

-- "repet1"
sized codata Traced (A : Set) (S : A -> Set) (R : A -> A -> Set) *(a : A) : 
  (i : Size) -> CoList A i -> Set
{ tstart : [i : Size] -> S a -> Traced A S R a $i (tnil i)
; tstep  : [i : Size] -> (a' : A) -> (t : CoList A i) ->  R a a' -> Traced A S R a' i t ->
    Traced A S R a $i (tcons i a' t)
}

{-
Goal  forall s, repet s -> exists t, repet1 s t.
-}

data Exists (A : Set) (B : A -> Set) : Set 
{ pair : (fst : A) -> (snd : B fst) -> Exists A B
} fields fst, snd

fun map2 : (A, A' : Set) -> (B : A -> Set) -> (B' : A' -> Set) ->
  (f : A -> A') -> (g : (a : A) -> B a -> B' (f a)) -> 
  Exists A B -> Exists A' B'
{ map2 A A' B B' f g (pair a b) = pair (f a) (g a b)
} 

let tcons_ : [A : Set] -> [i : Size] -> A -> CoList A i -> CoList A $i
  = \ A i a as -> tcons i a as

let tstep_ : [A : Set] -> [S : A -> Set] -> [R : A -> A -> Set] -> (a : A) ->
  [i : Size] -> (a' : A) -> (t : CoList A i) -> R a a' -> Traced A S R a' i t ->
    Traced A S R a $i (tcons i a' t)
  = \ A S R a i a' t r tr -> tstep i a' t r tr

cofun trace : (A : Set) -> (S : A -> Set) -> (R : A -> A -> Set) ->
   [i : Size] -> (a : A) -> Reach A S R a i ->
   Exists (CoList A i) (Traced A S R a i)
{ trace A S R ($i) a (start {-.A .S .R .a-} .i s) = 
    pair -- (CoList A $i) (Traced A S R a $i) 
      (tnil i) 
      (tstart {-A S R a-} i s)
; trace A S R ($i) a (step {-.A .S .R .a-} .i a' r x) =
    map2 (CoList A i) (CoList A $i)
         (Traced A S R a' i) (Traced A S R a $i)    
         (tcons_ A i a')
         (\ t -> tstep_ A S R a i a' t r)
         (trace A S R i a' x)
}      

{-
I expect that it requires classical reasoning & the axiom of choice,
but I have no clue as how to prove the statement in Coq.  Any help
will be mostly appreciated.

Thanks in advance,

Viktor


--

PS 1: If repet s were in Type instead of Prop, I would have defined

CoFixpoint witness s (H: repet s) :=
 match H with
 | repet_S SS => Tnil
 | repet_R s' RR H' => Tcons s' (witness s' H')
 end.

to provide a witness to the existential.
-}

