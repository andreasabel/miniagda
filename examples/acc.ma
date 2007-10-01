data Acc (A : Set) (Lt : A -> A -> Set) : A -> Set
{
  acc :  (b : A) ->
        ((a : A) -> Lt a b -> Acc A Lt a) 
        -> Acc A Lt b
} 

data Nat : Set  
{
	zero : Nat ;
	succ : Nat -> Nat
}

data Lt : Nat -> Nat -> Set
{
  ltzero : (y : Nat) -> Lt zero (succ y) ;
  ltsucc : (x : Nat) -> (y : Nat) -> Lt x y -> Lt (succ x) (succ y)
}

fun notLt0 : (x : Nat) -> Lt x zero -> (C : Set) -> C
{
-- notLt0 x  () C 
}


fun ltcase  : (x : Nat) -> (y : Nat) -> 
              Lt x (succ y) -> (P : Nat -> Set) -> ((x' : Nat) -> Lt x' y -> P x') -> P y -> P x
{
ltcase zero zero lt P  hx' hy = hy ;
ltcase zero (succ y) lt P hx' hy = hx' zero (ltzero y) ;
ltcase (succ x) (succ y) (ltsucc .x .(succ y) p) P hx' hy = ltcase x y p ( \n -> P (succ n)) ( \x' -> \p' -> hx' (succ x') (ltsucc x' y p')) hy
}

fun accSucc : (x : Nat) -> Acc Nat Lt x -> Acc Nat Lt (succ x)
{
accSucc x (acc .Nat .Lt .x h) = acc Nat Lt (succ x) (\y -> \p -> ltcase y x p (Acc Nat Lt) h (acc Nat Lt x h))
}

fun accLt : ( x : Nat ) -> Acc Nat Lt x
{

accLt zero = acc Nat Lt zero (\a -> \p -> notLt0 a p (Acc Nat Lt a) );
accLt (succ x) = accSucc x (accLt x)
}
