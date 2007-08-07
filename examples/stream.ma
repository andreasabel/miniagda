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
ones (s i) = cons i one (ones i)
}

const ones' : Stream Nat infty = ones infty

fun map : (A : Set) -> (B : Set) -> (i : Size) ->
          (A -> B) -> Stream A i -> Stream B i
{
map A B (s i) f (cons j a as) = cons i (f a) (map A B i f as)
} 

const twos : Stream Nat infty = map Nat Nat infty ( \ x -> succ x) ones

-- tail is a fun not a cofun
fun tail : (A : Set) -> (i : Size) -> Stream A (s i) -> Stream A i
{
tail  A i (cons j a as) = as
}

const twos' : Nat = tail Nat infty twos
-- evaluates to   map Nat Nat infty (\ (x : Nat) -> succ x) ones
 
-- head is a fun not a cofun
fun head : (A : Set) -> (i : Size) -> Stream A (s i) -> A
{
head A i (cons j a as) = a
}

const two' : Nat = head Nat infty twos
-- evaluates to two