-- 2013-04-08

data Bool { false ; true }
data Maybe (A : Set) { nothing ; just (fromJust : A) }

let Three = Maybe Bool
pattern one   = nothing
pattern two   = just false
pattern three = just true

data D (b : Three)
{ c : D three }

fun f : [b : Three] -> D b -> Set 1
{ f .three c = Set }
