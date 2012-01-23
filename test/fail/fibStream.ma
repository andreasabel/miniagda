
data Nat : Set {
  zero : Nat;
  succ : Nat -> Nat 
}

fun add : Nat -> Nat -> Nat {
  add zero = \y -> y;
  add (succ x) = \y -> succ (add x y)
}

sized codata Stream (+ A : Set) : Size -> Set {
  cons : (i : Size) -> A -> Stream A i -> Stream A ($ i)
}
 
fun tail : (A : Set) -> (i : Size) -> Stream A ($ i) -> Stream A i
{
  tail A i (cons .i x xs) = xs
}

cofun zipWith : (A : Set) -> (B : Set) -> (C : Set) ->
                (A -> B -> C) -> (i : Size) ->
		Stream A i -> Stream B i -> Stream C i 
{
  zipWith A B C f ($ i) (cons .i a as) (cons .i b bs) = 
	cons i (f a b)  (zipWith A B C f i as bs) 
}

let n0 : Nat = zero
let n1 : Nat = succ n0

-- although this is productive, matching ($ ($ i)) is disallowed for cofun
cofun fib : (i : Size) -> Stream Nat i
{
  fib ($ ($ i)) = cons _ n0 (cons _ n1 (zipWith Nat Nat Nat add
    i (fib i) (tail Nat i (fib ($ i)))))
}
