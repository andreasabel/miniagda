-- 2010-11-06

-- this might be accepted without trustme in future versions?!
trustme
data D : Set
{ abs : (^D -> D) -> D
}

-- this must fail!
fun app : ^D -> ^D -> D
{ app (abs f) d = f d
}
{- abs must not be characterized as a forced match!
only terminating eta-expansions are forced matches
-}
