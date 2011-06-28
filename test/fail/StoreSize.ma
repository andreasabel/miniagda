-- 2010-09-20 

data WrapSize : Set 1
{ inn : (out : Size) -> WrapSize
}
-- bug: MiniAgda tries to generate a destructor, even though out
-- is not a proper field (it is erased internally)