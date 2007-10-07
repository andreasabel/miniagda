-- streams without size

data Nat : Set  
{
	zero : Nat ;
	succ : Nat -> Nat
}

fun add : Nat -> Nat -> Nat
{
add x zero = x;
add x (succ y) = succ (add x y);
}

const one : Nat = succ zero

codata Stream (A : Set) : Set 
{
  cons : A -> Stream A -> Stream A 
}
 
cofun ones : Stream Nat
{
ones = cons Nat one ones 
}

const ones' : Stream Nat = ones

cofun map : (A : Set) -> (B : Set) ->
          (A -> B) -> Stream A -> Stream B
{
map .A B f (cons A a as) = cons B (f a) (map A B f as)
} 

const twos : Stream Nat = map Nat Nat ( \ x -> succ x) ones'

-- tail is a fun not a cofun
fun tail : (A : Set) -> Stream A -> Stream A 
{
tail .A (cons A a as) = as
}

const twos' : Stream Nat = tail Nat twos

-- evaluates to   map Nat Nat (\ (x : Nat) -> succ x) ones



-- head is a fun not a cofun
fun head : (A : Set) -> Stream A -> A
{
head .A (cons A a as) = a
}

const two : Nat = head Nat twos 
const two' : Nat = head Nat twos'
-- evaluates to two

cofun map' : (A : Set) -> (B : Set) ->
          (A -> B) -> Stream A -> Stream B 
{
map' A B = \f -> \as -> cons B (f (head A as)) (map' A B f (tail A as))
} 

const twos2 : Stream Nat = map' Nat Nat ( \ x -> succ x) ones'

const two2 : Nat = head Nat twos2

cofun zipWith : ( A : Set ) -> ( B : Set ) -> (C : Set) -> 
	(A -> B -> C) -> Stream A -> Stream B -> Stream C
{
zipWith .A .B C f (cons A a as) (cons B b bs) = cons C (f a b) (zipWith A B C f as bs)
}

fun nth : Nat -> Stream Nat -> Nat
{
nth zero ns = head Nat ns;
nth (succ x) ns = nth x (tail Nat ns) 
}

const fours : Stream Nat = zipWith Nat Nat Nat add twos twos

const four : Nat = head Nat fours

mutual{

cofun fibs : Stream Nat  
{
fibs = cons Nat zero (fibs') 
} 

cofun fibs' : Stream Nat
{
fibs' = cons Nat one (zipWith Nat Nat Nat add fibs' fibs)
}

}

cofun fibs2 : Stream Nat
{
fibs2 = cons Nat zero (cons Nat one (zipWith Nat Nat Nat add fibs (tail Nat fibs2)))
}
const fib8 : Nat = nth (add four four)  (fibs)
-- following hangs : 
--const fib8' : Nat = nth (add four four) (fibs2)

data SP ( A : Set ) ( B : Set ) : Set
{
put : B -> SP A B -> SP A B ; 
get : ( A -> SP A B) -> SP A B ; 
}

cofun mapSP : ( A : Set ) -> ( B : Set ) 
	-> (A -> B) -> SP A B 
{
mapSP A B f = get A B ( \a -> put A B (f a) (mapSP A B f)) 
}

cofun eat : ( A : Set ) -> ( B : Set ) -> 
	SP A B -> Stream A -> Stream B
{
eat .A .B (get A B f) (cons .A a as) = eat A B (f a) as;
eat .A .B (put A B b sp) (cons .A a as) = cons B b (eat A B sp (cons A a as))
}

fun adder : Nat -> Nat -> SP Nat Nat -> SP Nat Nat
{
adder zero acc sp = put Nat Nat acc sp;
adder (succ x) acc sp = get Nat Nat (\m -> adder x (add acc m) sp) 
}

cofun adder' : SP Nat Nat
{
adder' = get Nat Nat (\m -> adder m zero (adder'))
}

const fours' : Stream Nat = eat Nat Nat adder' twos'

const four' : Nat = nth (add one four) fours' 


cofun nats : Nat -> Stream Nat
{
nats x = (cons Nat x (nats (succ x)))
}

const nats' : Stream Nat = nats zero

const five : Nat = nth (add four one) nats'

const exp : Stream Nat = eat Nat Nat adder' nats'

const big : Nat = nth two exp

--- composition

fun comp : (A : Set ) -> ( B : Set ) -> ( C : Set ) -> SP A B -> SP B C -> SP A C
{
comp  A .B .C  t1            (put B C c t2) = put A C c (comp A B C t1 t2);
comp .A .B .C (put A B b t1) (get .B C f2)  = comp A B C t1 (f2 b) ;
comp .A .B  C (get A B f1)    t2            = get A C (\a -> comp A B C (f1 a) t2) 
} 

const adder2' : SP Nat Nat = comp Nat Nat Nat adder' adder'

const sixteens : Stream Nat = eat Nat Nat adder2' twos

const sixteen : Nat = nth one sixteens

-- an unproductive stream
cofun unp1 : Stream Nat 
{
unp1 = unp1
}

-- another one
cofun unp2 : Stream Nat
{
unp2 = cons Nat zero (tail Nat unp2)
}

--const bla : Nat = nth one unp2