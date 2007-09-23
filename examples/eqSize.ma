data Eq : Size -> Size -> Set
{
	refl : (i : Size) -> Eq i i 
}

--type checks with subtyping
const eqSucc : (i : Size ) -> Eq (s i) i = \j -> refl j 

  