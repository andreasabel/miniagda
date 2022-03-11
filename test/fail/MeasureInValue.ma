-- 2010-07-17

-- measures can only appear in fun-decls
-- caught by the scope-checker

fun f : (i,j : Size) -> |i| -> Set 1
{ f i j = |i| -> Set
}

