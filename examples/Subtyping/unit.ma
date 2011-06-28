-- 2011-06-15
-- the unit type as top type

data Top : Set { top : Top }

data Bool : Set { true : Bool; false : Bool }

-- this could work with coercive subtyping
-- record subtyping is hard at the moment since we have a nominal type
-- system

fun f : Bool -> Top
{ f x = x
}