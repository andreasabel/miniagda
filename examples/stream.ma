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
map .A B .(s i) f (cons A i a as) = cons B i (f a) (map A B i f as)
} 

const twos : Stream Nat infty = map Nat Nat infty ( \ x -> succ x) ones'

-- tail is a fun not a cofun
fun tail : (A : Set) -> (i : Size) -> Stream A (s i) -> Stream A i
{
tail .A .i (cons A i a as) = as
}

const twos' : Stream Nat infty = tail Nat infty twos

-- evaluates to   map Nat Nat infty (\ (x : Nat) -> succ x) ones



-- head is a fun not a cofun
fun head : (A : Set) -> (i : Size) -> Stream A (s i) -> A
{
head .A .i (cons A i a as) = a
}

const two : Nat = head Nat infty twos 
const two' : Nat = head Nat infty twos'
-- evaluates to two

cofun map' : (A : Set) -> (B : Set) -> (i : Size) ->
          (A -> B) -> Stream A i -> Stream B i
{
map' A B (s i) = \f -> \as -> cons B i (f (head A i as)) (map' A B i f (tail A i as))
} 

const twos2 : Stream Nat infty = map' Nat Nat infty ( \ x -> succ x) ones'

const two2 : Nat = head Nat infty twos2

cofun zipWith : ( A : Set ) -> ( B : Set ) -> (C : Set) -> ( i : Size ) ->
	(A -> B -> C) -> Stream A i -> Stream B i -> Stream C i
{
zipWith .A .B C .(s i) f (cons A i a as) (cons B .i b bs) = cons C i (f a b) (zipWith A B C i f as bs)
}

fun nth : Nat -> Stream Nat infty -> Nat
{
nth zero ns = head Nat infty ns;
nth (succ x) ns = nth x (tail Nat infty ns) 
}

const fours : Stream Nat infty = zipWith Nat Nat Nat infty add twos twos

const four : Nat = head Nat infty fours

mutual{

cofun fibs : (i : Size ) -> Stream Nat i  
{
fibs (s i) = cons Nat i zero (fibs' i) 
} 

cofun fibs' : (i : Size ) -> Stream Nat i
{
fibs' (s i) = cons Nat i one (zipWith Nat Nat Nat i add (fibs' i) (fibs i))
}

}

cofun fibs2 : ( i : Size ) -> Stream Nat i
{
fibs2 (s (s i)) = cons Nat (s i) zero (cons Nat i one (zipWith Nat Nat Nat i add (fibs2 i) (tail Nat i (fibs2 (s i)))))
}


const fib8 : Nat = nth (add four four)  (fibs infty)
-- following hangs : 
--const fib8' : Nat = nth (add four four) (fibs2 infty)


-- stream processor
data SP ( A : Set ) ( B : Set ) : Size -> Set
{
put : (i : Size ) -> B -> SP A B i -> SP A B (s i); 
get : (i : Size ) -> ( A -> SP A B (s i)) -> SP A B (s i); -- NB size arguments
}


-- execute stream processor
cofun eat : ( A : Set ) -> ( B : Set ) -> ( i : Size ) -> (j : Size)  
	-> SP A B i -> Stream A j -> Stream B infty
{
eat .A .B .(s i) .(s j) (get A B i f) (cons .A j a as) = eat A B (s i) j (f a) as;
eat .A .B .(s i) .(s j) (put A B i b sp) (cons .A j a as) = cons B infty b (eat A B i (s j) sp (cons A j a as))
}

-- eat n numbers and put sum 
 fun adder : (i : Size ) -> Nat -> Nat -> SP Nat Nat i -> SP Nat Nat (s i)
{
adder i zero acc sp = put Nat Nat i acc sp;
adder i (succ x) acc sp = get Nat Nat i (\m -> adder i x (add acc m) sp) 
}
cofun adder' : (i : Size ) -> SP Nat Nat (s i)
{
adder' (s i) = get Nat Nat (s i) (\m -> adder (s i) m zero (adder' i))
}

const fours' : Stream Nat infty = eat Nat Nat infty infty (adder' infty) twos'

const four' : Nat = nth (add one four) fours' 


cofun nats : (i : Size ) -> Nat -> Stream Nat i
{
nats (s i) x = (cons Nat i x (nats i (succ x)))
}

const nats' : Stream Nat infty = nats infty zero

const five : Nat = nth (add one four) nats'

const exp : Stream Nat infty = eat Nat Nat infty infty (adder' infty) nats'

const big : Nat = nth two exp

--- stream processor composition
fun comp : (i : Size ) -> ( j : Size ) -> (A : Set ) -> ( B : Set ) -> ( C : Set )
	 -> SP A B i -> SP B C j -> SP A C infty
{
comp  i .(s j)  A .B .C  t1          (put B C j c t2)  = put A C infty c (comp i j A B C t1 t2);
comp .(s i) .(s j) .A .B .C (put A B i b t1) (get .B C j f2)  = comp i (s j) A B C t1 (f2 b) ;
comp .(s i)  j .A .B  C (get A B i f1)    t2               = get A C infty (\a -> comp (s i) j A B C (f1 a) t2) 
} 

const adder2' : SP Nat Nat infty = comp infty infty Nat Nat Nat (adder' infty) (adder' infty)

const sixteens : Stream Nat infty = eat Nat Nat infty infty adder2' twos

const sixteen : Nat = nth one sixteens


--- weakening
--  but incomplete pattern  
cofun wkStream : ( A : Set ) -> ( i : Size ) -> Stream A (s i) -> Stream A i
{
wkStream .A .(s i) (cons A (s i) x xs) = cons A i x (wkStream A i xs)
}

-- bad 
-- not admissble ? 
cofun wkStream2 : ( A : Set ) -> ( i : Size ) -> Stream A i -> Stream A (s i)
{
wkStream2 .A .(s i) (cons A i x xs) = cons A (s i) x (wkStream2 A i xs)
}

-- an unproductive stream
cofun unp : (i : Size ) -> Stream Nat i 
{
unp i = unp i
}

-- another one
cofun unp2 : (i : Size ) -> Stream Nat i
{
unp2 (s i) = cons Nat i zero (tail Nat i (unp2 (s i)))
}
-- const bla : Nat = nth one (unp2 infty)