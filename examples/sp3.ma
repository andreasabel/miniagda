-- stream processors

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
  cons : (i : Size ) -> Nat -> Stream i -> Stream ($ i) 
}

fun tail : (i : Size ) -> Stream ($ i) -> Stream i 
{
tail .i (cons i a as) = as
}


fun head : (i : Size ) -> Stream ($ i) -> Nat
{
head .i (cons i a as) = a
} 


mutual {

data ISP (i : Size ) : Set
{
put : Nat -> SP i -> ISP i ; 
get : (Nat -> ISP i) -> ISP i; 
}

-- nice, but currently not a sized type
codata SP : Size -> Set
{
isp : (i : Size ) -> ISP i -> SP ($ i);
}

}

mutual {

fun ieat : (i : Size ) -> ISP # -> Stream # -> Stream ($ i)
{
ieat i (get .# f) (cons .# a as) = ieat i (f a) as ;
ieat i (put .# b sp) as  = cons i b (eat i sp as) 
}

cofun eat : (i : Size ) -> SP # -> Stream # -> Stream i 
{
eat ($ i) (isp .# sp) as = ieat i sp as   
}

}


fun adder : (i : Size ) -> Nat -> Nat -> SP i -> ISP i 
{
adder i zero acc sp = put i acc sp;
adder i (succ n) acc sp = get i (\ m -> (adder i n (add m acc) sp))
}

cofun adder' : (i : Size ) -> SP i 
{
adder' ($ i) = isp i (get i (\ n -> adder i n zero (adder' i)))
}

fun nth : Nat -> Stream # -> Nat
{
nth zero ns = head # ns;
nth (succ x) ns = nth x (tail # ns) 
}

cofun twos : (i : Size ) -> Stream i
{
twos ($ i) = cons i (succ (succ zero)) (twos i)
}

-- stream of fours
const fours : Stream # = eat # (adder' #) (twos #) 

eval const four : Nat = nth two fours

