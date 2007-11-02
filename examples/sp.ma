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

const one : Nat = succ zero
const two : Nat = succ one

codata Stream : Size -> Set 
{
  cons : (i : Size) -> Nat -> Stream i -> Stream ($ i)
}

fun tail : (i : Size) -> Stream ($ i) -> Stream i
{
tail .i (cons i a as) = as
}


fun head : (i : Size) -> Stream ($ i) -> Nat
{
head .i (cons i a as) = a
} 

-- continuation style

-- inductive stream processor
data ISP ( + K : Set) : Set
{
put : Nat -> K -> ISP K; 
get : (Nat -> ISP K) -> ISP K; 
}

codata SP : Size -> Set
{
sp : (i : Size) -> ISP (SP i) -> SP ($ i);
}

fun ieat : (K : Set) -> (C : Set) -> 
           ISP K -> Stream # -> (Nat -> K -> Stream # -> C) -> C 
{
ieat .K C (get K f) (cons .# a as) h = ieat K C (f a) as h ;
ieat .K C (put K b k)          as  h = h b k as 
}

cofun eat : (i : Size) -> SP # -> Stream # -> Stream i
{
eat ($ i) (sp .# isp) as 
  = ieat (SP #) (Stream ($ i))
         isp as (\ b -> \ k -> \ as' -> 
                   cons i b (eat i k as'))   
}

fun adder : Nat -> Nat -> (K : Set ) -> ( k : K) -> ISP K
{
adder zero acc K k = put K acc k;
adder (succ n) acc K k = get K (\ m -> (adder n (add m acc) K k))
}

cofun adder' : (i : Size ) -> SP i
{
adder' ($ i) = sp i (get (SP i) (\ n -> adder n zero (SP i) (adder' i)))
}

cofun nats : (i : Size ) -> Nat -> Stream i
{
nats ($ i) x = (cons i x (nats i (succ x)))
}

const huge : Stream # = eat # (adder' #) (nats # zero) 

fun nth : Nat -> Stream # -> Nat
{
nth zero ns = head # ns;
nth (succ x) ns = nth x (tail # ns) 
}

eval const big : Nat = nth (add two two) huge 

cofun twos : (i : Size ) -> Stream i
{
twos ($ i) = cons i (succ (succ zero)) (twos i)
}

-- stream of fours
const fours : Stream # = eat # (adder' #) (twos #)

eval const four : Nat = nth two fours

