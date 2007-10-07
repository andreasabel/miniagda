-- not a sized type..
data Eq : Size -> Size -> Set
{
	refl : (i : Size) -> Eq i i 
}

--so this does not work with subtyping
const eqSucc : (i : Size ) -> Eq ($ i) i = \j -> refl j 

  