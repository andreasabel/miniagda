-- date: 2010-06-03
-- modified: 2010-09-26

data Eq (A : Set)(a : A) : A -> Set
{ refl : Eq A a a
}

{- Greetings,

I was playing with CoInductive types in Coq and found this problem:
-----------------

Require Import Logic.

(*if you define the type of streams of units:*)

CoInductive Stream : Set :=
| cons : Stream -> Stream.

CoFixpoint ones : Stream := cons ones.
-}

sized codata Stream : Size -> Set 
{ cons : [i : Size] -> (tail : Stream i) -> Stream ($ i)
}
-- tail : [i : Size] -> Stream ($ i) -> Stream i

cofun ticks : [i : Size] -> Stream i
{ ticks ($ i) = cons i (ticks i)
}
-- tail i (ticks .$i) = ticks i

{- (*and a function out such that out (cons xs) := cons xs *)

Definition out (xs : Stream) : Stream :=
match xs with
| cons ys => cons ys
end. -}

fun eta : Stream # -> Stream #
{ eta (cons .# s) = cons # s
}
-- 1st step: definition by destructors
-- tail .# (eta (cons .# s)) = s
-- 2nd step: translating irrefutable pattern (cons .# s) = s' --> s = tail # s'
-- tail .# (eta s') = tail # s' 

{- (*Then you can prove: forall xs, xs = out xs *)

Lemma out_eq (xs : Stream) : xs = out xs .
intros xs.
case_eq xs.
intros s eq.
reflexivity.
Defined. -}

fun ext : (xs : Stream #) -> Eq (Stream #) xs (eta xs)
{ ext (cons .# xs') = refl (Stream #) (cons # xs')
}
-- 2010-09-26
-- ext xs = refl (Stream #) (cons # (tail # xs))
-- this does not type check with destructors!

{- (* This seems not to be a problem, but we can deduce *)

Definition p : ones = cons ones := out_eq ones. -}

let p : Eq (Stream #) (ticks #) (cons # (ticks #))
      = ext (ticks #)

{- (* Now, if we want to see how this is proved: *)

Eval compute in p. (* returns refl_equal (cons (cofix ones : Stream := cons ones)) *)

(* but if we feed back the normal form *) -}

let p2 : Eq (Stream #) (ticks #) (cons # (ticks #))
       = refl (Stream #) (ticks #)


{- Definition p2 : ones = cons ones :=
refl_equal (cons (cofix ones : Stream := cons ones)).

(* the type checker yells that ones and cons ones are not convertible *)
-}