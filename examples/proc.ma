data Nat : Set 
{
zero : Nat;
succ : Nat -> Nat;
}

-- nondet. oracle
codata C : Set
{
ff : C -> C;
tt : C -> C
}

codata Proc : Set
{
nil : Proc ;
out : Nat -> Proc -> Proc;
in : (Nat -> Proc ) -> Proc 
}


-- unreliable proc that forgets input on ff
cofun m : C -> Proc
{
m (ff c) = in (\n -> (m c)); 
m (tt c) = in (\n -> out n (m c));
}

-- fairness

--predicate Event1(x,y) <=> x = ff ( ff ( ... tt ( y) ))
 
data Event1 : C -> C -> Set
{
d1 : ( x : C ) -> Event1 (tt x) x;
d2 : ( x : C) -> ( y : C ) -> Event1 x y -> Event1 (ff x) (ff y);
}

-- predicate Inf x <=> x has infinitly many tts...

codata Inf1 : C -> Set
{
inf1 : (x : C) -> ( y : C ) -> (Event1 x y) -> Inf1 y -> Inf1 x
}

-- a fair medium has infinitly many outs
data Fairmed : Proc -> Set
{
fairmed : ( x : C ) -> (m1 : C -> Proc ) -> Inf1 x -> Fairmed (m1 x)  
}

cofun ones : C 
{
ones = tt ones
}

const lemma_ones : Event1 ones ones = d1 ones

cofun proof : Inf1 ones
{
proof = inf1 ones ones lemma_ones proof 
}

-- reliable medium
const relmed : Fairmed (m ones) = fairmed ones m proof  





