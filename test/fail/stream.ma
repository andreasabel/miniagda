
sized codata Stream : Size -> Set {
  cons : (i : Size) -> Nat -> Stream i -> Stream ($ i)
}

fun tail : Stream # -> Stream # {
  tail (cons .# x xs) = xs
}

fun head : Stream # -> Nat {
  head (cons .# x xs) = x
}


 
cofun lookbad : (i : Size ) -> Stream i
{
lookbad ($ i) = 
	first (Stream _) (Stream _) 
	  (cons _ zero (lookbad _))
          (lookbad _)
}

--let proof2 : Eq (Stream #) (cons # zero (lookbad #)) (lookbad #) = refl (Stream #) (lookbad #)
--let proof3 : Eq (Stream #) (cons # zero (lookbad #)) (tail (lookbad #)) = refl (Stream #) (tail (lookbad #)

let proof2 : Eq (Stream #) (cons # zero (lookbad #)) (lookbad #) = refl (Stream #) (lookbad #)
let proof3 : Eq (Stream #) (cons # zero (lookbad #)) (tail (lookbad #)) = refl (Stream #) (tail (lookbad #))

