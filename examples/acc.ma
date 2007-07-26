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
  P 0 0 (ltzero 0) hx' hy = hy ;
  P 0 (succ y) (ltzero (succ y)) hx' hy = hx' 0 (ltzero y) ;
  P (succ x) (succ y) (ltsucc x y p) hx' hy =  ??? ;
}

fun notLt0 : (x : Nat) -> Lt x zero -> (C : Set) -> C
{
  x () C 
}

fun accWk : (i : Size) -> (x : Nat) -> (y : Nat) -> 
            Lt x y -> Acc Nat Lt (s i) x -> Acc Nat Lt i y
{
  i x y p (acc i x pacc) = pacc y p 
} 

fun accLt : (x : Nat) -> Acc Nat Lt infty x
{
  zero = acc infty zero 
             (\ (a : Nat) -> \ (p : Lt a zero) -> notLt0 a p (Acc Nat Lt infty a)) ;
  (succ x) = acc infty (succ x) (\ (a : Nat) -> \ (p : Lt a (succ x)) ->
                                 ltcase
                                    (Acc Nat Lt infty) a (succ x) p 
                                    (accWk infty a x p (accLt x))
                                    (accLt x))
}