data Nat : Set  
{
	zero : Nat ;
	succ : Nat -> Nat
}

const one : Nat = succ zero

codata Stream (A : Set) : Size -> Set 
{
  cons : (i : Size) -> A -> Stream A i -> Stream A (s i)
}
 
cofun ones : (i : Size) -> Stream Nat i
{
ones (s i) = cons Nat i one (ones i)
}

const ones' : Stream Nat infty = ones infty

cofun map : (A : Set) -> (B : Set) -> (i : Size) ->
          (A -> B) -> Stream A i -> Stream B i
{
map A B (s i) f (cons .A .i a as) = cons B i (f a) (map A B i f as)
} 

const twos : Stream Nat infty = map Nat Nat infty ( \ x -> succ x) ones'

-- tail is a fun not a cofun
fun tail : (A : Set) -> (i : Size) -> Stream A (s i) -> Stream A i
{
tail  A i (cons .A .i a as) = as
}

const twos' : Stream Nat infty = tail Nat infty twos

-- evaluates to   map Nat Nat infty (\ (x : Nat) -> succ x) ones
 
-- head is a fun not a cofun
fun head : (A : Set) -> (i : Size) -> Stream A (s i) -> A
{
head A i (cons .A .i a as) = a
}

const two' : Nat = head Nat infty twos
-- evaluates to two

cofun map' : (A : Set) -> (B : Set) -> (i : Size) ->
          (A -> B) -> Stream A i -> Stream B i
{
map' A B (s i) = \f -> \as -> cons A i (f (head A i as)) (map' A B i f (tail A i as))
} 

const twos2 : Stream Nat infty = map' Nat Nat infty ( \ x -> succ x) ones'

const two2 : Nat = head Nat infty twos2
