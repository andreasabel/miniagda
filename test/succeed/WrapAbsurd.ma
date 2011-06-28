-- 2010-07-08

data Wrap ++(A : Set) : Set
{ wrap : (unwrap : A) -> Wrap A
}

data Empty : Set {}

-- should succeed
fun wrap0Elim : Wrap Empty -> Empty
{ wrap0Elim (wrap .Empty ()) 
}

data Unit : Set { unit : Unit }

-- should fail
fail fun wrap1Elim : Wrap Unit -> Empty
{ wrap1Elim (wrap .Unit ())
}

{- BEFORE BUG FIX:

checkPattern
  dot pats: [(0,(Unit,[(Set 0)]))]
  environ : [(".Unit",v0)]
  context : [[(Set 0)]]
  pattern : ()
  at type : ((unwrap : v0) -> Wrap A{A = v0})	<>

the test whether there are matchingConstructors is too optimistic
since v0 is not solved yet to be Unit, it finds no matching constructors
--> it should solve first

BUG FIX: postpone emptyness check till after pattern checking
-}