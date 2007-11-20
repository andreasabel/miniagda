data Nat : Set {
  zero : Nat;
  succ : Nat -> Nat 
}

fun add : Nat -> Nat -> Nat {
  add zero = \y -> y;
  add (succ x) = \y -> succ (add x y)
}

codata Stream : Size -> Set {
  cons : (i : Size) -> Nat -> Stream i -> Stream ($ i)
}
 
fun tail : (i : Size) -> Stream ($ i) -> Stream i {
  tail .i (cons i n ns) = ns
}

fun head : (i : Size) -> Stream i -> Nat {
  head .($ i) (cons i n ns) = n
}

cofun zipWith :  (Nat -> Nat -> Nat ) -> ( i : Size ) 
		-> Stream i -> Stream i -> Stream i {
  zipWith f ($ i) as bs = 
	cons i (f (head i as) (head i bs))  (zipWith f i (tail i as) (tail i bs)) 
}

fun nth : Nat -> Stream # -> Nat {
  nth zero ns = head # ns;
  nth (succ x) ns = nth x (tail # ns) 
}

cofun fibs : ( i : Size ) -> Stream i
{
  fibs ($ $ i) = cons ($ i) zero (cons i (succ zero)
	 	(zipWith add i (fibs i) (tail i (fibs ($ i)))))
}


mutual {

cofun fibs2 : ( i : Size ) -> Stream i
{
  fibs2 ($ i) = cons i zero (fibs2' i)
}

cofun fibs2' : (i : Size ) -> Stream i 
{

fibs2' ($ i) = cons i (succ zero) (zipWith add i (fibs2 i) (fibs2' i))
}

}


const 4 : Nat = (succ (succ (succ (succ zero))))
-- fib(4) = 3 
eval const fib4 : Nat = nth 4 (fibs2 #) 

data Eq (A : Set ) : A -> A -> Set
{
refl : (a : A ) -> Eq A a a
}

-- const proof : Eq (Stream #) (tail # (fibs2 #)) (tail # (fibs2 #)) = refl (Stream #) (tail # (fibs2 #))



const double : (i : Size ) -> Stream i -> Stream ($ i) = \i -> \s -> cons i (head i s) s

cofun z : (i : Size ) -> Stream i
{
z ($ i) = double i (z i)
}

eval const zzz : Nat = head # (z #) 





