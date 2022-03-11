-- 2011-06-15
-- the unit type as top type

record Top : Set { top : Top }

data Bool : Set { true : Bool; false : Bool }

-- this could work with coercive subtyping
-- record subtyping is hard at the moment since we have a nominal type
-- system

-- 2012-01-28 works now (only for unit and empty type)
fun f : Bool -> Top
{ f x = x
}
