data Nat : Set 
{
zero : Nat;
succ : Nat -> Nat;
}

-- nondet. oracle
codata C : Size -> Set
{
ff : ( i : Size ) -> C i -> C (s i);
tt : ( i : Size ) -> C i -> C (s i);
}

codata Proc : Set
{
nil : Proc ;
out : Nat -> Proc -> Proc;
in : (Nat -> Proc ) -> Proc 
}

-- unreliable proc that forgets input on ff
cofun m : ( i : Size ) -> C i -> Proc
{
m .(s i) (ff i c) = in (\n -> (m i c)); 
m .(s i) (tt i c) = in (\n -> (out n (m i c)))
}

const m' : C infty -> Proc = m infty

-- fairness

--predicate Event1(x,y) <=> x = ff ( ff ( ... tt ( y) ))
 
data Event1 : C infty -> C infty -> Set
{
d1 : ( x : C infty) -> Event1 (tt infty x) x;
d2 : ( x : C infty ) -> ( y : C infty ) -> Event1 x y -> Event1 (ff infty x) (ff infty y);  
}

-- predicate Inf x <=> x has infinitly many tts...
-- sized for termination of proof
codata Inf1 : Size -> C infty -> Set
{
inf1 : (i : Size ) -> (x : C infty ) -> ( y : C infty) -> (Event1 x y) -> Inf1 i y -> Inf1 (s i) x
}

-- a fair medium has infinitly many outs
data Fairmed : Proc -> Set
{
fairmed : ( x : C infty ) -> (m1 : C infty -> Proc ) -> Inf1 infty x -> Fairmed (m1 x)  
}

cofun ones : ( i : Size ) -> C i
{
ones (s i) = tt i (ones i)
}

const ones' : C infty = ones infty

const lemma_ones  : Event1 ones' ones' = d1 ones'


cofun proof : (i : Size ) -> Inf1 i ones'
{
proof (s i) = inf1 i ones' ones' lemma_ones  (proof i)
}

-- reliable medium
const relmed : Fairmed (m' ones') = fairmed ones' m' (proof infty)  





