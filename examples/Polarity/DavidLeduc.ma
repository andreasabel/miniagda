{- 2010-09-20, David Leduc on the Agda list.  Agda's positivity checker rejects this:

open import Coinduction

data unit : Set where
 * : unit

mutual
 data T : Set1 where
   intro : (t : \infty T) -> [ \flat t ] -> T

 [_] : T -> Set
 [ intro _ _ ] = unit

-}

data Unit : Set { unit : Unit }

mutual {

  codata T : Set 1
  { intro : (t : T) -> bracket t -> T
  }

  fun bracket : T -> Set
  { bracket (intro t b) = Unit
  }

}

