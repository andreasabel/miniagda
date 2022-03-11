{- 2011-11-23
I created a function gcd of type
[i : Size] -> [j : Size] -> Nat i -> Nat j -> Nat (i + j)
One of my cases pattern matches on arguments
gcd i j (succ (i > k) x) (succ (j > l) y)
and tries to make a recursive call
gcd l k y x
This generates a constraint k + l + 0 <= i + j, which fails to be solved by the typechecker.
But since k < i and l < j we have k + l + 0 = k + l <= i + j, so it seems to me that it should
succeed. Is this a bug? Am I missing something?
-}

sized data Nat : Size -> Set
{ zero : [i : Size] -> Nat ($ i)
; succ : [i : Size] -> Nat i -> Nat ($ i)
}

fun gcd0 : [i : Size] -> Nat i -> Nat i -> Nat i
{ gcd0 i (succ (i > k) x) (succ (i > l) y) = gcd0 (max l k) y x
}

{- constraint not solved
fun gcd : [i : Size] -> [j : Size] -> Nat i -> Nat j -> Nat (i + j)
{ gcd i j (succ (i > k) x) (succ (j > l) y) = gcd l k y x
}
-}
