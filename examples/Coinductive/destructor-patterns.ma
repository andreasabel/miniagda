-- 2010-09-26
-- getting sized destructor patterns right
-- no matching on size

cofun fib : [i : Size] -> Stream Nat i
{ fib i = cons (i > j) zero         -- bind j on rhs!
           (cons (j > k) (suc zero)   -- bind k
             (zipWith (+) k (fib k) (tail k (fib j))))
}

cofun fib : [i : Size] -> Stream Nat i
{               head (i > j) (fib i)  = zero
; head (j > k) (tail (i > j) (fib i)) = suc zero
; tail (j > k) (tail (i > j) (fib i)) = zipWith (+) k (fib k) (tail k (fib j))
}
{- checking ok

k < j < i |- 
  fib j : Stream Nat j
  fib j : Stream Nat $k  -- by subtyping
  tail k (fib j) : Stream Nat k

-}
cofun map f : [i : Size] -> Stream A i -> Stream B i
{ head (i > j) (map f i s) =       f (head j s)
; tail (i > j) (map f i s) = map f j (tail j s)
}
{- checking

s : Stream A i, j < i |-
  s        : Stream A $j  -- by subtyping
  tail j s : Stream A j
  map ...  : Stream A j
-}

cofun id2 : [i : Size] -> Stream A i -> Stream A i
{               head (i > j) (id2 i s)  = head j s
; head (j > k) (tail (i > j) (id2 i s)) = head k (tail j s)
; tail (j > k) (tail (i > j) (id2 i s)) = id2 k (tail k (tail j s))
}
{- s : Stream A i,  k < j < i |- rhs ok!

seems fine, no paradox
-}
 
{-    
cofun even : [i : Size] ? Stream (2 * i) ? Stream i
{ even (1 + i) (cons (2 + 2 * i) x (cons (1 + 2 * i) y t))
= cons i x (even (2 * i) t)
}
-}

cofun evens : [i : Size] -> Stream A (2i) -> Stream A i
{ head (i > j) (evens i s) = head (2j + 1) s
; tail (i > j) (evens i s) = evens j (tail (2j) (tail (2j+1) s)) 
}

{- s : Stream 2i <= Stream 2(j+1)
   evens j (t (t s)) : Stream j
-}

 