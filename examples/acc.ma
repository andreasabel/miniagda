data Acc (A : Set) (Lt : A -> A -> Set) : Size -> A -> Set
{
  acc : (i : Size) -> (b : A) ->
        ((a : A) -> Lt a b -> Acc A Lt i a) 
        -> Acc A Lt (s i) b
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

fun ltcase  : (P : Nat -> Set) -> (x : Nat) -> (y : Nat) -> 
              Lt x (succ y) -> ((x' : Nat) -> Lt x' y -> P x') -> P y -> P x
{
ltcase  P zero zero (ltzero zero) hx' hy = hy ;
ltcase  P zero (succ y) (ltzero (succ y2)) hx' hy = hx' zero (ltzero y) ;
ltcase  P (succ x) (succ y) (ltsucc x2 y2 p) hx' hy =  x -- ??? ;
}

fun notLt0 : (x : Nat) -> Lt x zero -> (C : Set) -> C
{
notLt0 x  () C 
}

fun accWk : (i : Size) -> (x : Nat) -> (y : Nat) -> 
            Lt x y -> Acc Nat Lt (s i) x -> Acc Nat Lt i y
{
accWk  i x y p (acc i2 x2 pacc) = pacc y p 
} 

fun accLt : (x : Nat) -> Acc Nat Lt infty x
{
accLt zero = acc infty zero 
             (\ a -> \ p -> notLt0 a p (Acc Nat Lt infty a)) ;
accLt (succ x) = acc infty (succ x) (\ a -> \ p ->
                                 ltcase
                                    (Acc Nat Lt infty) a (succ x) p 
                                    (accWk infty a x p (accLt x))
                                    (accLt x))
}