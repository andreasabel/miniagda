-- 2013-04-02  Careful with $# = # !

cofun D : Size -> Set
{ D i = [j < i] -> |i| < |$$j| -> D j -> D j
}
-- This constrains j = $i.
--
-- Note that D # = [j < #] -> # < $$j -> D j -> D j
-- should never be usable, since there is no such j.
--
-- However, if MiniAgda does not honour that i < $i is only valid
-- for i < #, then we can create a looping program.

fun app_ : [i : Size] -> D $i -> D i -> D i
{ app_ i f = f i
}
-- Here, f of type
--
--   [j < $i] -> |$i| < |$$j| -> D j -> D j
--
-- is instantiated with i < $i,
-- which is invalid with for i = # under # = $#, to
--
--   |$i| < |$$i| -> D i -> D i
--
-- which is just  D i -> D i.
--
-- Thus app_ should only have type [i < #] -> D $i -> D i -> D i.
-- In this case, we could not make the following instantiation i = #.

let app : D # -> D # -> D # = app_ #

fun abs_ : [i : Size] -> (D i -> D i) -> D $i
{ abs_ i f j x = f x  -- x : D j <= D i  (since i$ <= $j) [j < $i]
}
-- Here, we have
--
--   j < $i
--   $i < $$j
--   f : D i -> D i
--   x : D j
--
-- Subtyping x   : D j <= D i  needs i = j
-- Subtyping f x : D i <= D j  needs i = j
--
-- i = j is derived from
--   $i < $$j which entails i <= j (valid)
--   j < $i   which entails j <= i (valid)
--
-- When introducing j < $i then $i < $$j is eagerly introduced into
-- the context.  Here, MiniAgda should complain since the latter
-- constraint is only satisfiable if i < #.
--
-- Thus, abs_ should also be restricted to type [i < #],
-- making the following instantiation impossible.

let abs : (D # -> D #) -> D # = abs_ #

-- Now we can construct the usual counterexample to normalization of
-- untyped lambda-calculus.

let delta : D # = abs (\ x -> app x x)
let Omega : D # = app delta delta
-- evaluation of Omega loops
