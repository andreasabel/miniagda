-- 2010-09-23 see DavidLeduc.txt

data Unit : Set { unit : Unit }

mutual {
 
  codata T : Set 1 
  { makeT : (set : Set) -> (content : T) -> (property : El content) -> T
  }

  fun El : T -> Set
  { El (makeT A t p) = A
  }

}

-- this gives an error because the definition of t is not available during t.c.
fail cofun t : T
{ t = makeT Unit t unit
} 

-- Ulf's fix

{- TODO fix scope checker
mutual {

  cofun t : T
  { t = makeT Unit t u
  }

  cofun u : El t -- t not in scope here in MiniAgda
  { u = unit
  }

}
-}