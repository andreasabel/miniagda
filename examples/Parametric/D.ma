-- 2010-11-06

-- this might be accepted without trustme in future versions?!
trustme
data D : Set
{ abs : (^D -> D) -> D
}

fun app : D -> ^D -> D
{ app (abs f) d = f d
}
{-
fun app : ^D -> ^D -> D  -- problem: this checks
{ app (abs f) d = f d
}

abs must not be characterized as a forced match!
only terminating eta-expansions are forced matches
-}

fun sapp : D -> D
{ sapp x = app x x
}

-- this needs to fail, since x is not parametric in the application!
fail
let delta : D
  = abs (\ x -> sapp x)
