data Nat : Set 
{
zero : Nat;
succ : Nat -> Nat;
}

-- nondet. oracle
sized codata C : Size -> Set
{
ff : ( i : Size ) -> C i -> C ($ i);
tt : ( i : Size ) -> C i -> C ($ i);
}

sized codata Proc : Size -> Set
{
nil : (i : Size ) -> Proc ($ i);
out : (i : Size ) -> Nat -> Proc i -> Proc ($ i);
ins :  (i : Size ) -> (Nat -> Proc i) -> Proc ($ i) 
}

-- unreliable proc that forgets input on ff
cofun m : ( i : Size ) -> C # -> Proc i
{
m ($ i) (ff .# c) = ins i (\n -> (m i c)); 
m ($ i) (tt .# c) = ins i (\n -> (out i n (m i c)))
}

let m' : C # -> Proc # = m #

-- fairness

--predicate Event1(x,y) <=> x = ff ( ff ( ... tt ( y) ))
 
data Event1 : C # -> C # -> Set
{
d1 : ( x : C #) -> Event1 (tt # x) x;
d2 : ( x : C # ) -> ( y : C # ) -> Event1 x y -> Event1 (ff # x) (ff # y);  
}

-- predicate Inf x <=> x has infinitly many tts...
-- sized for termination of proof
sized codata Inf1 : Size -> C # -> Set
{
inf1 : (i : Size ) -> (x : C # ) -> ( y : C #) -> (Event1 x y) -> Inf1 i y -> Inf1 ($ i) x
}

-- a fair medium has infinitly many outs
data Fairmed : Proc # -> Set
{
fairmed : ( x : C # ) -> (m1 : C # -> Proc # ) -> Inf1 # x -> Fairmed (m1 x)  
}

cofun ones : ( i : Size ) -> C i
{
ones ($ i) = tt i (ones i)
}

let ones' : C # = ones #

let lemma_ones  : Event1 ones' ones' = d1 ones'


cofun proof : (i : Size ) -> Inf1 i ones'
{
proof ($ i) = inf1 i ones' ones' lemma_ones (proof i)
}

-- reliable medium
let relmed : Fairmed (m' ones') = fairmed ones' m' (proof #)  





