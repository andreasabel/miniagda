-- 2010-08-31 shadowing test

-- no complaint here, because constructor name introduced after checking its sig
data D (name : Set) : Set
{ name : D name
}

-- usage fine
fun id : [A : Set] -> D A -> D A
{ id A (name) = name
}

-- but complaint here, because constructor name in scope
-- 2010-10-01 this is now fine!
data E (name : Set) : Set
{ e : E name
}

-- a bit weird, still...
