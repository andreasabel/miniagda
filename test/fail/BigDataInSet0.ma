-- 2010-09-20

data BigOk : Set 1
{ bigOk : Set 0 -> BigOk
}

-- 2012-10-10 suceeds, because of irrelevance
data BigIrr : Set
{ bigIrr : .Set -> BigIrr
}

data Big : Set 0
{ big : Set 0 -> Big
}
-- needs to fail, constructor lives in Set 1
