data Nat : Set
{
--zero : Nat;
--succ : Nat -> Nat
}

-- rec. argument appears positivly 
data Good1 : Set
{
good1 : (Nat -> Good1) -> Good1
}

-- rec. argument appears negativly
data Bad1 : Set
{
-- bad1 : (Bad1 -> Nat) -> Bad1
}

-- rec. argument appears not - positivly
data Bad2 : Set
{
-- bad2 : ((Bad2 -> Nat ) -> Nat) -> Nat -> Bad2
}

-- rec. argument appears positivly 
data Good2 : Set
{
  good2 : (Nat -> Nat -> Good2) -> Good2
}

-- bad, we don't know anything about F
data Bad3 ( A: Set ) ( F : Set -> Set ) : Set 
{
-- bad3 : A -> F (Bad3 A F) -> Bad3 A F
}


-- truly nested is not allowed
data Bush ( + A: Set ) : Set
{
-- bnil : A -> Bush A;
-- bcons : Bush (Bush A) -> Bush A
}

--------- parameters that are declared to be strcicly positive need to be strictly positive ...

data Bad4 ( + A : Set ) : Set 
{
--bad4 : (A -> Nat) -> Bad4 A -> Bad4 A
}

-- ok, of course
data List ( + A : Set ) : Set 
{
nil : A -> List A;
cons : A -> List A -> List A
}


-- ok because we know List has strily pos. parameter
data Tree ( + A : Set ) : Set
{
node : A -> List (Tree A) -> Tree A
}




