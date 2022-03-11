data Nat : Set
{
        zero : Nat ;
        succ : (x : Nat) -> Nat
}

fun add : Nat -> Nat -> Nat
{
add x zero = x;
add x (succ y) = succ (add x y);
}

eval let one : Nat = succ zero

sized codata Stream (A : Set) : Size -> Set
{
  cons : (i : Size) -> A -> Stream A i -> Stream A ($ i)
}

cofun zeroes : (i : Size ) -> Stream Nat i
{
zeroes ($ i) = cons i zero (zeroes i)
}

cofun ones : (i : Size) -> Stream Nat i
{
ones ($ i) = cons i one (ones i)
}

eval let ones' : Stream Nat # = ones #

cofun map : (A : Set) -> (B : Set) -> (i : Size) ->
          (A -> B) -> Stream A # -> Stream B i
{
map A B ($ i) f (cons .# a as) = cons i (f a) (map A B i f as)
}

eval let twos : Stream Nat # = map Nat Nat # ( \ x -> succ x) ones'



-- tail is a fun
fun tail : (A : Set) -> Stream A # -> Stream A #
{
tail A (cons .# a as) = as
}


eval let twos' : Stream Nat # = tail Nat twos

fun head : (A : Set) -> Stream A # -> A
{
head A (cons .# a as) = a
}

eval let two : Nat = head Nat twos
eval let two' : Nat = head Nat twos'

eval let twos2 : Stream Nat # = map Nat Nat # succ ones'
eval let twos2' : Stream Nat # = tail Nat twos2

cofun zipWith : ( A : Set ) -> ( B : Set ) -> (C : Set) -> ( i : Size ) ->
        (A -> B -> C) -> Stream A # -> Stream B # -> Stream C i
{
zipWith A B C ($ i) f (cons .# a as) (cons .# b bs) =
  cons i (f a b) (zipWith A B C i f as bs)
}



fun nth : Nat -> Stream Nat # -> Nat
{
nth zero ns = head Nat ns;
nth (succ x) ns = nth x (tail Nat ns)
}

eval let fours : Stream Nat # = zipWith Nat Nat Nat # add twos twos
eval let four : Nat = head Nat fours



cofun fib : (x : Nat ) -> (y : Nat ) -> (i : Size ) -> Stream Nat i
{
fib x y ($ i) = (cons ($ i) x (cons i y (fib y (add x y) i)))
}

eval let fib' : Stream Nat # = tail Nat (fib zero zero #)


eval let fib8 : Nat = nth (add four four) (fib zero zero #)

eval let fib2 : Nat  = head Nat (tail Nat (fib zero zero #))

cofun nats : (i : Size ) -> Nat -> Stream Nat i
{
nats ($ i) x = (cons i x (nats i (succ x)))
}

eval let nats' : Stream Nat # = tail Nat (nats # zero)


--- weakening
eval let wkStream : ( A : Set ) -> ( i : Size ) -> Stream A ($ i) -> Stream A i = \ A -> \ i -> \ s -> s

-- should be ok but does not pass admissibility check
cofun wkStream_ok : ( A : Set ) -> (i : Size ) -> Stream A ($ i) -> Stream A i
{
wkStream_ok A ($ i) (cons .($ i) x xs) = cons i x (wkStream A i xs)
}

