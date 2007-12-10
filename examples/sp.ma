data Nat : Set
{
zero : Nat;
succ : Nat -> Nat
}

fun add : Nat -> Nat -> Nat
{
add zero y = y;
add (succ x) y = succ (add x y)
}

let one : Nat = succ zero
let two : Nat = succ one

sized codata Stream : Size -> Set 
{
  cons : (i : Size) -> Nat -> Stream i -> Stream ($ i)
}

fun tail : Stream # -> Stream #
{
tail (cons .# a as) = as
}


fun head : Stream # -> Nat
{
head (cons .# a as) = a
} 

-- continuation style

-- inductive stream processor
data ISP ( + K : Set) : Set
{
put : Nat -> K -> ISP K; 
get : (Nat -> ISP K) -> ISP K; 
}

-- coinductive stream processor
sized codata SP : Size -> Set
{
isp : (i : Size) -> ISP (SP i) -> SP ($ i);
}


fun ieat : (K : Set) -> (C : Set) -> 
           ISP K -> Stream # -> (Nat -> K -> Stream # -> C) -> C 
{
ieat .K C (get K f) (cons .# a as) h = ieat K C (f a) as h ;
ieat .K C (put K b k)          as  h = h b k as 
}

cofun eat : (i : Size) -> SP # -> Stream # -> Stream i
{
eat ($ i) (isp .# ip) as 
  = ieat (SP #) (Stream ($ i))
         ip as (\ b -> \ k -> \ as' -> 
                   cons i b (eat i k as'))   
}


fun iadder : Nat -> Nat -> (K : Set ) -> K -> ISP K
{
iadder zero acc K k = put K acc k;
iadder (succ n) acc K k = get K (\ m -> (iadder n (add m acc) K k))
}

cofun adder' : (i : Size ) -> SP i
{
adder' ($ i) = isp i (get (SP i) (\ n -> iadder n zero (SP i) (adder' i)))
}

let adder : SP # = adder' #


fun nth : Nat -> Stream # -> Nat
{
nth zero ns = head ns;
nth (succ x) ns = nth x (tail ns) 
}


-- 2 , 2 , ...
cofun twos : (i : Size ) -> Stream i
{
twos ($ i) = cons i (succ (succ zero)) (twos i)
}

-- executing adder on the stream 2 , 2 ... produces the stream 4 , 4 , ...
let fours : Stream # = eat # adder (twos #)

eval let four : Nat = head fours

