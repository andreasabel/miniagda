-- 2012-01-22 parameters gone from constructors

data Nat : Set {
  zero : Nat;
  succ : (n : Nat) -> Nat
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

{-
norec head : (i : Size) -> Stream ($ i) -> Nat {
  head .($ i) (cons i n ns) = n
}

cofun zipWith :  (Nat -> Nat -> Nat ) -> ( i : Size )
                -> Stream i -> Stream i -> Stream i {
  zipWith f ($ i) as bs =
        cons i (f (head i as) (head i bs))  (zipWith f i (tail i as) (tail i bs))
}
-}

fun nth : Nat -> Stream # -> Nat {
  nth zero xs = head xs;
  nth (succ x) xs = nth x (tail xs)
}

let one : Nat = (succ zero)

cofun fib' : (x : Nat ) -> (y : Nat ) -> (i : Size ) -> Stream i
{
  fib' x y ($ i) = cons _ x (fib' y (add x y) _)
}
let fib : Stream # = (fib' one one _)


let four : Nat = (succ (succ (succ one)))

-- fib(four) = 5
eval let fibfour : Nat = nth four fib


--------------------------------------------
--------------------------------------------

data Leq : Nat -> Nat -> Set
{
lqz : (x : Nat ) -> Leq zero x ;
lqs : (x : Nat ) -> (y : Nat ) -> Leq x y -> Leq (succ x) (succ y)
}

sized codata Increasing : Size -> Stream # -> Set
{
inc : (i : Size ) -> (x : Nat ) -> (y : Nat ) -> Leq x y -> (tl : Stream # ) ->
      Increasing i (cons # y tl) ->
      Increasing ($ i) (cons # x (cons # y tl))
}


data Eq (+ A : Set)(a : A) : A -> Set
{ refl : Eq A a a
}

let proof : Eq (Stream #) (tail fib) (tail fib) = refl



let double : Stream # -> Stream # = \s -> cons _ (head s) s

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
      ite (le x y) (Stream _)
         (cons _ x (merge _ le xs (cons _ y ys)))
         (cons _ y (merge _ le (cons _ x xs) ys))
}

fun first : (A : Set ) -> (B : Set ) -> A -> B -> A
{
first A B a b = a
}

--------------------

cofun map : (i : Size) -> (Nat -> Nat) -> Stream i -> Stream i
{
map ($ i) f (cons .i x xl) = cons _ (f x) (map _ f xl)
}

{-
-- 2012-01-22 constructor are no longer inferable!
let suc : Nat -> Nat = \ x -> succ x
-- 2012-01-25 constructor recognition also for function types
-}

cofun evil : (i : Size) -> Stream i
{
evil ($ i) = map _ succ (cons _ zero (evil _))
}

-- eval const zzz : Nat = head # (z #)

