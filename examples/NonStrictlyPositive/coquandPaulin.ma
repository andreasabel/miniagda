let Prop = Set 0

trustme -- ignore strict positivity warning
data I +(i : Size) : Set 1
{ C [j < i] (h : (I j -> Prop) -> Prop)
}

data Id [a : Size][A : Set a](x : A) : A -> Set
{ refl : Id a A x x
}

let f [i : Size] (p, q : I i -> Prop) : Prop
  = Id 1 (I i -> Prop) p q

let inj [i : Size] (p : I i -> Prop) : I (i + 1)
  = C i (f i p)

let inj0 [i < #] (p : I i -> Prop) : I (i + 1)
  = C i (f i p)

-- Infinity

let f' (p, q : I # -> Prop) : Prop
  = Id 1 (I # -> Prop) p q

fail
fun inj' : (I # -> Prop) -> I #
{ inj' p = C # (f p)
}

fail
let inj'' (p : I # -> Prop) : I #
  = C # (f p)

let inj3 : (I # -> Prop) -> I #
  = inj #

-- -- Constructing Omega

-- let abs = inj3

-- let app (x, y : I #) : Prop
--   = Id 1 (I #) x y

-- let sapp (x : I # -> Prop) 
--   = app x (inj3 x)