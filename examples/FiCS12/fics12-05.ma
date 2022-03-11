-- 2012-02-05  Fibonacci stream in deflatinary iteration style

data Nat { zero ; succ (n : Nat) }

fun add : Nat -> Nat -> Nat
{ add zero     y = y
; add (succ x) y = succ (add x y)
}

cofun Stream : +(A : Set) -(i : Size) -> Set
{ Stream A i = [j < i] -> A & Stream A j
}

let cons [A : Set] [i : Size] (a : A) (as : Stream A i) : Stream A $i
  = \ j -> (a, as)


fun head : [A : Set] -> [i : Size] -> Stream A $i -> A
{ head A i s = case s 0 { (a, as) -> a }
}

fun tail : [A : Set] -> [i : Size] -> Stream A $i -> Stream A i
{ tail A i s j = case s $j { (a, as) -> as j }
}


cofun zipWith : [A, B, C : Set] -> (A -> B -> C) ->
                [i : Size] -> Stream A i -> Stream B i -> Stream C i
{ zipWith A B C f i sa sb j =
  case (sa j, sb j) : (A & Stream A j) & (B & Stream B j)
  { ((a, as), (b, bs)) -> (f a b, zipWith A B C f j as bs)
  }
}

let adds : [i : Size] -> Stream Nat i -> Stream Nat i -> Stream Nat i
  = zipWith Nat Nat Nat add

let one : Nat = succ zero

check
cofun fib : [i : Size] -> Stream Nat i
{ fib i j = zero, \ k -> (one, adds k (fib j) (tail Nat k (fib j)))
}

cofun fib : [i : Size] -> |i| -> Stream Nat i
{ fib i = \ j -> zero,
          \ k -> succ zero,
          zipWith Nat Nat Nat add k
            (fib k)
            (tail Nat k (fib j))
}

-- we have given up strong normalization

-- prohibiting reduction under lambda would restore it
-- but then we have to switch to pi-sigma style intensional equality
-- of functions, no more eta
-- eta only in Conor's propositional observational equality

-- or, we embrace destructor patterns
