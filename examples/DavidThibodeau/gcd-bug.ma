sized data SNat : Size -> Set
{ zero : (i : Size) -> SNat ($ i)
; succ : (i : Size) -> SNat i -> SNat ($ i)
}


{- I have a concern. I tweaked the sizes and obtained that my
implementation is terminating.  The problem appears in the following
example that passes the termination check: -}

fail
fun test : [i : Size] -> SNat i -> SNat i -> SNat i
{ test i (succ (i > k) x) (succ (k > l) y) = test i (succ l y) (succ k x)
}

{- It asserts that succ y has size l which is smaller than k so that
when it recursively calls test with the argument reversed, the first
argument decreases in size. But the algorithm would then pattern match
with the only case available and again swap the two arguments.  It
seems to me that this algorithm cannot be terminating. Is it a bug?
Should I not be able to write such function?  -}
