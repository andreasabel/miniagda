-- 2012-02-04

-- Putting the bound on the outside ensures positivity
check
cofun D : +(i : Size) -> Set
{ D i = [j < i] & (D j -> D j)
}

-- If we place the bound directly before the recursive occurrence
-- we need strictly positive functionals

cofun D : +(i : Size) -> Set
{ D i = [j < i] & D j -> [j < i] & D j
}
-- fails