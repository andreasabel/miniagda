-- 2010-11-01

-- Nat ---------------------------------------------------------------

sized data SNat : Size -> Set 
{ zero : [i : Size] -> SNat ($ i)
; succ : [i : Size] -> SNat i -> SNat ($ i) 
}

let Nat : Set = SNat #

fun add : Nat -> Nat -> Nat 
{ add (zero .#)   = \ y -> y
; add (succ .# x) = \ y -> succ # (add x y)
}

-- Stream ------------------------------------------------------------

sized codata Stream ++(A : Set) : -Size -> Set 
{ cons : [i : Size] -> (head : A) -> (tail : Stream A i) -> Stream A ($ i)
}
fields head, tail

cofun zipWith : [A : Set] -> [B : Set] -> [C : Set] ->
                (A -> B -> C) -> [i : Size] ->
		Stream A i -> Stream B i -> Stream C i 
{
  zipWith A B C f ($ i) (cons .A .i a as) (cons .B .i b bs) = 
	cons C i (f a b)  (zipWith A B C f i as bs) 
}


-- Fibonacci stream --------------------------------------------------

let n0 : Nat = zero #
let n1 : Nat = succ # n0

cofun fib : [i : Size] -> Stream Nat i
{
  fib ($ i) = cons Nat i n0 (zipWith Nat Nat Nat add i 
    (cons Nat i n1 (fib i)) (fib i))
}

cofun fib2 : [i : Size] -> Stream Nat (i + i)
{
  fib2 ($ i) = -- RHS illtyped, produces only Stream Nat $(i + i)
    cons Nat (i + i) n0 
      (zipWith Nat Nat Nat add (i + i) 
        (cons Nat (i + i) n1 (fib2 i)) 
        (fib2 i))
}

