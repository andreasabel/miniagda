data Nat : Set {
  zero : Nat;
  succ : Nat -> Nat 
}

fun add : Nat -> Nat -> Nat {
  add zero = \y -> y;
  add (succ x) = \y -> succ (add x y)
}

sized codata Stream : Size -> Set {
  cons : (i : Size) -> Nat -> Stream i -> Stream ($ i)
}
 
fun tail : Stream # -> Stream # {
  tail (cons .# x xs) = xs
}

fun head : Stream # -> Nat {
  head (cons .# x xs) = x
}

fun nth : Nat -> Stream # -> Nat {
  nth zero xs = head xs;
  nth (succ x) xs = nth x (tail xs) 
}

const 1 : Nat = (succ zero)

cofun fib' : (x : Nat ) -> (y : Nat ) -> (i : Size ) -> Stream i {
  fib' x y ($ i) = cons i x (fib' y (add x y) i)
} 
const fib : Stream # = (fib' 1 1 #)


const 4 : Nat = (succ (succ (succ 1)))

-- fib(4) = 5 
eval const fib4 : Nat = nth 4 fib 



--------------------------------------------
--------------------------------------------

data Leq : Nat -> Nat -> Set
{
lqz : (x : Nat ) -> Leq zero x ;
lqs : (x : Nat ) -> (y : Nat ) -> Leq x y -> Leq (succ x) (succ y)
}

sized codata Increasing : Size -> Stream # -> Set
{
inc : (i : Size ) -> (x : Nat ) -> (y : Nat ) -> Leq x y -> (tl : Stream # ) -> Increasing i (cons # y tl) ->
    Increasing ($ i) (cons # x (cons # y tl)) 
}


data Eq (A : Set ) : A -> A -> Set
{
refl : (a : A ) -> Eq A a a
}

const proof : Eq (Stream #) (tail fib) (tail fib) = refl (Stream #) (tail fib)



const double : Stream # -> Stream # = \s -> cons # (head s) s

data Bool : Set 
{
tt : Bool;
ff : Bool
}

fun leq : Nat -> Nat -> Bool
{
leq zero y = tt;
leq (succ x) zero = ff ;
leq (succ x) (succ y) = leq x y 
}

fun ite : Bool -> (A : Set ) -> A -> A -> A
{
ite tt A a1 a2 = a1;
ite ff A a1 a2 = a2
}

cofun merge : (i : Size ) -> Stream # -> Stream # -> Stream i
{
merge ($ i) (cons .# x xs) (cons .# y ys) = 
      ite (leq x y) (Stream ($ i))
         (cons i x (merge i xs (cons # y ys)))
	 (cons i y (merge i (cons # x xs) ys))     
}

fun first : (A : Set ) -> (B : Set ) -> A -> B -> A
{
first A B a b = a
}

cofun lookbad : (i : Size ) -> Stream i
{
lookbad ($ i) = 
	first (Stream ($ i)) (Stream i) 
	  (cons i zero (lookbad i))
          (lookbad i)
}

--const proof2 : Eq (Stream #) (cons # zero (lookbad #)) (lookbad #) = refl (Stream #) (lookbad #)
--const proof3 : Eq (Stream #) (cons # zero (lookbad #)) (tail (lookbad #)) = refl (Stream #) (tail (lookbad #))

--------------------

cofun map : (i : Size ) -> (Nat -> Nat) -> Stream i -> Stream i 
{
map .($ i) f (cons i x xl) = cons i (f x) (map i f xl)
}

cofun evil : (i : Size ) -> Stream i
{
evil ($ i) = map ($ i) succ (cons i zero (evil i))
}

eval const e : Nat = head (evil #)





