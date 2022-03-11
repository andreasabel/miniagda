-- Absurd pattern used on non-empty type

data Unit : Set
{ unit : Unit }

fun bla : Unit -> Set
{ bla ()
}
