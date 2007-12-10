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

let 1 : Nat = (succ zero)

cofun fib' : (x : Nat ) -> (y : Nat ) -> (i : Size ) -> Stream i {
  fib' x y ($ i) = cons i x (fib' y (add x y) i)
} 
let fib : Stream # = (fib' 1 1 #)


let 4 : Nat = (succ (succ (succ 1)))

-- fib(4) = 5 
eval let fib4 : Nat = nth 4 fib 


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


data Eq (+ A : Set ) : A -> A -> Set
{
refl : (a : A ) -> Eq A a a
}

let proof : Eq (Stream #) (tail fib) (tail fib) = refl (Stream #) (tail fib)



let double : Stream # -> Stream # = \s -> cons # (head s) s

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

cofun merge : (i : Size ) -> (Nat -> Nat -> Bool) -> Stream # -> Stream # -> Stream i
{
merge ($ i) le (cons .# x xs) (cons .# y ys) = 
      ite (le x y) (Stream ($ i))
         (cons i x (merge i le xs (cons # y ys)))
	 (cons i y (merge i le (cons # x xs) ys))     
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

let proof2 : Eq (Stream #) (cons # zero (lookbad #)) (lookbad #) = refl (Stream #) (lookbad #)
let proof3 : Eq (Stream #) (cons # zero (lookbad #)) (tail (lookbad #)) = refl (Stream #) (tail (lookbad #))



