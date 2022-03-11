-- 2012-01-22 parameters gone from constructors

data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

fun add : Nat -> Nat -> Nat
{ add zero     = \ y -> y
; add (succ x) = \ y -> succ (add x y)
}

sized codata Stream (+ A : Set) : Size -> Set {
  cons : [i : Size] -> A -> Stream A i -> Stream A ($ i)
}

fun head : [A : Set] -> [i : Size] -> Stream A ($ i) -> A
{ head A i (cons .i a as) = a
}

fun tail : [A : Set] -> [i : Size] -> Stream A ($ i) -> Stream A i
{ tail A i (cons .i a as) = as
}

cofun zipWith : [A : Set] -> [B : Set] -> [C : Set] -> (A -> B -> C) ->
                [i : Size] -> Stream A i -> Stream B i -> Stream C i
{ zipWith A B C f ($ i) (cons .i a as) (cons .i b bs) =
    cons i (f a b)  (zipWith A B C f i as bs)
}

cofun adds : [i : Size] -> Stream Nat i -> Stream Nat i -> Stream Nat i
{ adds ($ i) (cons .i a as) (cons .i b bs) =
    cons i (add a b) (adds i as bs)
}

let one : Nat = succ zero

{- Size matching

at type  [i : Size] -> Co i  one can match i against ($ j)
since for i = 0,  Co i is the set of all terms

type checking rule

    i:Size, j < i, i --> $ j |- e : Gamma -> Co ($ j)
    -------------------------------------------------
    case i { ($ j) -> e } : Gamma -> Co i

basically, there is an analysis whether the type of the case is
"everything"  (opposite of empty).

 -}

cofun fib' : [i : Size] -> Stream Nat i
{
  fib' i = case i
   { ($ j) -> cons j zero (case j
   { ($ k) -> cons k one (zipWith Nat Nat Nat add k
                              (fib' k)
                              (tail Nat k (fib' ($ k))))})}
}

{- we can pull one case into the pattern match, but not both -}

cofun fib : [i : Size] -> Stream Nat i
{ fib ($ i) = cons i zero (case i
    { ($ j) -> cons j one (adds j (fib j) (tail Nat j (fib i)))})
}

{- blueprint
cofun fib : [i : Size] -> Stream Nat i
{ fib ? = cons ? zero
    (cons ? one (adds ? (fib ?) (tail Nat ? (fib ?))))
}
-- UNSOUND
cofun fib : [i : Size] -> Stream Nat i
{ fib ($$ i) = cons ($ i) zero
    (cons i one (adds i (fib i) (tail Nat ($ i) (fib ($ i)))))
}
-}

{- the question is how to facilitate inference for this?
   We need to insert case splits at the appropriate positions.
   Why not, this is a form of type reconstruction.
   Relies on bidirectional type checking.
   Currently, MiniAgda does not check constructors, but infers them, which is bad.
 -}
