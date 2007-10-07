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

codata Stream (A : Set) : Size -> Set 
{
  cons : (i : Size) -> A -> Stream A i -> Stream A ($ i)
}
 
cofun ones : (i : Size) -> Stream Nat i
{
ones ($ i) = cons Nat i one (ones i)
}


const ones' : Stream Nat # = ones #

cofun map : (A : Set) -> (B : Set) -> (i : Size) ->
          (A -> B) -> Stream A i -> Stream B i
{
map .A B .($ i) f (cons A i a as) = cons B i (f a) (map A B i f as)
} 

const twos : Stream Nat # = map Nat Nat # ( \ x -> succ x) ones'

-- tail is a fun not a cofun
cofun tail : (A : Set) -> (i : Size) -> Stream A ($ i) -> Stream A i
{
tail .A .i (cons A i a as) = as
}

const twos' : Stream Nat # = tail Nat # twos

-- head is a fun not a cofun
--fun head : (A : Set) -> (i : Size) -> Stream A ($ i) -> A
--{
--head .A .i (cons A i a as) = a
--}

-- head is a fun not a cofun
fun head2 : (A : Set) -> Stream A # -> A
{
head2 .A (cons A .# a as) = a
}

const two : Nat = head2 Nat twos 
const two' : Nat = head2 Nat twos'
-- evaluates to two

const twos2 : Stream Nat # = map Nat Nat # ( \ x -> succ x) ones'


cofun zipWith : ( A : Set ) -> ( B : Set ) -> (C : Set) -> ( i : Size ) ->
	(A -> B -> C) -> Stream A i -> Stream B i -> Stream C i
{
zipWith A B C ($ i) f (cons .A .i a as) (cons .B .i b bs) = cons C i (f a b) (zipWith A B C i f as bs)
}



fun nth : Nat -> Stream Nat # -> Nat
{
nth zero ns = head2 Nat ns;
nth (succ x) ns = nth x (tail Nat # ns) 
}

const fours : Stream Nat # = zipWith Nat Nat Nat # add twos twos

const four : Nat = head2 Nat fours

mutual{

cofun fibs : (i : Size ) -> Stream Nat i  
{
fibs ($ i) = cons Nat i zero (fibs' i) 
} 

cofun fibs' : (i : Size ) -> Stream Nat i
{
fibs' ($ i) = cons Nat i one (zipWith Nat Nat Nat i add (fibs' i) (fibs ($ i)))
}

}

cofun fibs2' : ( i : Size ) -> Stream Nat i
{
fibs2' ($ $ i) = cons Nat ($ i) zero (cons Nat i one (zipWith Nat Nat Nat i add (fibs2' i) (tail Nat i (fibs2' ($ i)))))
--fibs2' ($ i) = cons Nat i one (zipWith Nat Nat Nat i add (fibs2' i) (cons Nat i zero (fibs2' i))) 
}


const fib8 : Nat = nth (add four four)  (fibs #)
const fib8' : Nat = nth (add four four) (fibs2' #) 


-- stream processor
data SP ( A : Set ) ( B : Set ) : Size -> Set
{
put : (i : Size ) -> B -> SP A B i -> SP A B ($ i); 
get : (i : Size ) -> ( A -> SP A B ($ i)) -> SP A B ($ i); -- NB size arguments
}






cofun nats : (i : Size ) -> Nat -> Stream Nat i
{
nats ($ i) x = (cons Nat i x (nats i (succ x)))
}

const nats' : Stream Nat # = nats # zero


--- weakening
cofun wkStream : ( A : Set ) -> ( i : Size ) -> Stream A ($ i) -> Stream A i
{
wkStream .A .($ i) (cons A ($ i) x xs) = xs -- w/o subtyping : cons A i x (wkStream A i xs)
}

-- bad 
-- not admissble ? 
--cofun wkStream2 : ( A : Set ) -> ( i : Size ) -> Stream A i -> Stream A ($ i)
--{
--wkStream2 .A .($ i) (cons A i x xs) = cons A ($ i) x (wkStream2 A i xs)
--}

-- an unproductive stream
cofun unp : (i : Size ) -> Stream Nat i 
{
unp i = unp i
}

-- another one
cofun unp2 : (i : Size ) -> Stream Nat i
{
unp2 ($ i) = cons Nat i zero (tail Nat i (unp2 ($ i)))
}

-- looking at a finite part of an unproductive stream hangs
--const bla : Nat = nth one (unp2 #)

mutual
{

cofun evens : ( i : Size ) -> Stream Nat i
{
evens ($ i) = cons Nat i zero (map Nat Nat i succ (odds i))
}

cofun odds : ( i : Size ) -> Stream Nat i
{
odds i = map Nat Nat i succ (evens i)
}

}


-- not guarded by constructor
cofun nats2 : ( i : Size) -> Stream Nat i
{
nats2 ($ i) = cons Nat i zero (map Nat Nat i succ (nats2 i)) 
}
