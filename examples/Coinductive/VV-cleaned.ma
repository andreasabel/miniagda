-- 2011-01-19
{- Message of ViktorVafeiadis on the Coq mailing list, 2011-01-18

Dear Coq-club,

I am stuck proving the following goal:

Variable A : Type.
Variable R : A -> A -> Prop.
Variable S : A -> Prop.
-}

fun A : Set {}
fun R : A -> A -> Set {}
fun S : A -> Set {}

{-
CoInductive trace: Type :=
 | Tnil
 | Tcons (x: A) (t: trace).
-}

-- "trace" is the type of colists over A
sized codata CoList : Size -> Set 
{ tnil  : [i : Size] -> CoList $i
; tcons : [i : Size] -> A -> CoList i -> CoList $i
}

{-
CoInductive repet (s : A) : Prop :=
 | repet_S : S s -> repet s
 | repet_R : forall s', R s s' -> repet s' -> repet s.
-}
 
-- "repet" is what I call "Reach"
sized codata Reach *(a : A) : Size -> Set
{ start : [i : Size] -> S a -> Reach a $i
; step  : [i : Size] -> (a' : A) -> R a a' -> Reach a' i -> Reach a $i
}

{-
CoInductive repet1 (s : A) : trace -> Prop :=
 | repet1_S : S s -> repet1 s Tnil
 | repet1_R : forall s' t, R s s' -> repet1 s' t -> repet1 s (Tcons s' t).

(* repet1 is just like repet, but has an extra argument remembering
  the trace of intermediate states. *)
-}

-- "repet1"
sized codata Traced *(a : A) : (i : Size) -> CoList i -> Set
{ tstart : [i : Size] -> S a -> Traced a $i (tnil i)
; tstep  : [i : Size] -> (a' : A) -> (t : CoList i) ->  R a a' -> 
    Traced a' i t -> Traced a $i (tcons i a' t)
}

-- tuple type with bimap
data Exists (X : Set) (Y : X -> Set) : Set 
{ pair : (fst : X) -> (snd : Y fst) -> Exists X Y
} fields fst, snd

fun map2 : (X, X' : Set) -> (Y : X -> Set) -> (Y' : X' -> Set) ->
  (f : X -> X') -> (g : (x : X) -> Y x -> Y' (f x)) -> 
  Exists X Y -> Exists X' Y'
{ map2 X X' Y Y' f g (pair x y) = pair (f x) (g x y)
} 

{-
Goal  forall s, repet s -> exists t, repet1 s t.
-}

let tcons_ : [i : Size] -> A -> CoList i -> CoList $i
  = \ i a as -> tcons i a as

let tstep_ : (a : A) -> [i : Size] -> (a' : A) -> (t : CoList i) -> 
    R a a' -> Traced a' i t -> Traced a $i (tcons i a' t)
  = \ a i a' t r tr -> tstep i a' t r tr

cofun trace : [i : Size] -> (a : A) -> Reach a i -> Exists (CoList i) (Traced a i)
{ trace ($i) a (start .i s) = 
    pair -- (CoList $i) (Traced a $i) 
      (tnil i) 
      (tstart i s)
; trace ($i) a (step .i a' r x) =
    map2 (CoList i) (CoList $i)
         (Traced a' i) (Traced a $i)    
         (tcons_ i a')
         (\ t -> tstep_ a i a' t r)
         (trace i a' x)
}      

{-
I expect that it requires classical reasoning & the axiom of choice,
but I have no clue as how to prove the statement in Coq.  Any help
will be mostly appreciated.

Thanks in advance,

Viktor
-}

-- No, it is just coinduction!
-- Andreas
