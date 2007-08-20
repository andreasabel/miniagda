data Nat : Set 
{
	zero : Nat;
	succ : Nat -> Nat
}

fun add : Nat -> Nat -> Nat
{
add x zero = x;
add x (succ y) = succ (add x y)
}

const one : Nat = succ zero
const two : Nat = succ one

data Bool : Set
{
	tt : Bool ;
	ff : Bool 
}

fun ite : (A : Set ) -> Bool -> A -> A -> A
{
ite A tt x y = x;
ite A ff x y = y
}

codata Stream ( A : Set ) : Size -> Set
{
	cons : (i : Size ) -> A -> Stream A i -> Stream A (s i) 
}

cofun ones : (i : Size ) -> Stream Nat i 
{
ones (s i) = cons Nat i one (ones i)
}

const ones' : Stream Nat infty = ones infty

cofun map : (i : Size ) -> ( A : Set ) -> ( B : Set ) -> Stream A i -> (A -> B) -> Stream B i
{
map (s i) A B (cons .A .i a as) f = cons B i (f a) (map i A B as f)
}

const twos : Stream Nat infty = map infty Nat Nat ones' (\x -> succ x ) 

fun tail : ( i : Size ) -> ( A : Set ) -> Stream A (s i) -> Stream A i 
{
tail i A (cons .A .i a as) = as
}

fun head : (i : Size ) -> ( A : Set ) -> Stream A i -> A
{
head (s i) A (cons .A .i a as) = a
}

const one' : Nat = head infty Nat ones'

cofun merge : (i : Size ) -> (j : Size ) -> ( A : Set ) -> (A -> A -> Bool) -> Stream A i -> Stream A j -> Stream A infty
{
merge (s i) (s j) A le (cons .A .i a as) (cons .A .j b bs) = ite (Stream A infty) (le a b)
                           (cons A infty a (merge i (s j) A le as (cons A j b bs)))
                           (cons A infty b (merge (s i) j A le (cons A i a as) bs))
}



data TransMu (A : Set) ( B : Set) (X : Set) : Set
{
  put : B -> X -> TransMu A B X;
  get : (A -> TransMu A B X) -> TransMu A B X
}

fun transMu : (i : Size) -> (A : Set) -> ( B : Set) -> (X : Set ) -> 
          TransMu A B X -> Stream A i -> (B -> X -> Stream A i -> Stream B i) -> Stream B i
{
transMu (s i) A B X (get .A .B .X f) (cons .A .i a as) k = transMu (s i) A B X (f a) as k;
transMu i A B X (put .A .B .X b x) as k = k b x as
}

cofun Trans : Set -> Set -> Set
{
Trans A B = TransMu A B (Trans A B) 
}


cofun repeat : ( i : Size ) -> (A : Set) -> A -> Stream A i 
{
repeat (s i) A a = cons A i a (repeat i A a)
}

cofun trans : ( i : Size ) -> ( A : Set) -> ( B : Set)  -> Trans A B -> Stream A i -> Stream B i
{
-- trans (s i) A B (put .A .B .(Trans A B) b t) as = cons B (s i) b (trans i A B t as)
trans (s i) A B (get .A .B .(Trans A B) f) (cons .A .i a as) = transMu i A B (Trans A B) (f a) as
                                          (\ b -> \ t -> \ as' -> cons B i b (trans i A B t as'))
}

const TMu : Set -> Set = TransMu Nat Nat 


fun eatAdding : (X : Set) -> Nat -> Nat -> X -> TMu X
{
eatAdding X zero     acc x = put Nat Nat X acc x;
eatAdding X (succ n) acc x = get Nat Nat X (\ m -> eatAdding X n (add m acc) x)
}

cofun adder : Trans Nat Nat 
{
adder = get Nat Nat (Trans Nat Nat) (\ n -> eatAdding (Trans Nat Nat) n zero (adder))
}

const test : Stream Nat infty = trans infty Nat Nat adder (repeat infty Nat (succ (succ zero)))

const four : Nat = head infty Nat test

cofun natsf : (i : Size ) -> Nat -> Stream Nat i
{
natsf (s i) x = cons Nat i x (natsf i (succ x))
}

const nats : Stream Nat infty = natsf infty zero

fun nth : Nat -> Stream Nat infty -> Nat
{
nth zero ns = head infty Nat ns;
nth (succ x) ns = nth x (tail infty Nat ns) 
}

const test2 : Stream Nat infty = trans infty Nat Nat adder nats

const t0 : Nat = nth zero test2
const t1 : Nat = nth one test2
const t2 : Nat = nth two test2
const t3 : Nat = nth (succ two) test2
const t4 : Nat = nth four test2

 
