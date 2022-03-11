-- bug reported by David Thibodeau, Nov 2011
-- fixed 2012-01-24

sized data SNat : Size -> Set
{ zero : (i : Size) -> SNat ($ i)
; succ : (i : Size) -> SNat i -> SNat ($ i)
}

-- the following should fail:

fun test : [i : Size] -> SNat i -> SNat i -> SNat i
{ test i (succ (i > k) x) (succ (k > l) y) = test i (succ l y) (succ k x)
}
-- the second successor pattern has not the correct upper bound (k instead i)
