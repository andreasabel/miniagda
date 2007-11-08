-- unsized stream processors

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

codata Stream : Set 
{
  cons : Nat -> Stream -> Stream 
}

fun tail : Stream  -> Stream 
{
tail (cons a as) = as
}


fun head : Stream  -> Nat
{
head (cons a as) = a
} 


mutual {

data ISP : Set
{
put : Nat -> SP -> ISP ; 
get : (Nat -> ISP ) -> ISP ; 
}

codata SP : Set
{
isp : ISP -> SP;
}

}

mutual {

fun ieat : ISP -> Stream -> Stream
{
ieat (get f) (cons a as) = ieat (f a) as ;
ieat (put b sp) as  = cons b (eat sp as) 
}

cofun eat : SP -> Stream -> Stream 
{
eat (isp sp) as = ieat sp as   
}

}

fun adder : Nat -> Nat -> SP -> ISP 
{
adder zero acc sp = put acc sp;
adder (succ n) acc sp = get (\ m -> (adder n (add m acc) sp))
}

cofun adder' : SP 
{
adder' = isp (get (\ n -> adder n zero adder'))
}

fun nth : Nat -> Stream -> Nat
{
nth zero ns = head ns;
nth (succ x) ns = nth x (tail ns) 
}

cofun twos : Stream 
{
twos = cons (succ (succ zero)) twos
}

-- stream of fours
const fours : Stream = eat adder' twos 

eval const four : Nat = nth two fours

mutual {

fun comp : ISP -> ISP -> ISP
{
comp t1 (put c t2) = put c (comp3 t1 t2);
comp (put b t1) (get f2) = comp2 t1 (f2 b);
comp (get f1) t2 = get (\a -> comp (f1 a) t2) 
}

fun comp2 : SP -> ISP -> ISP
{
comp2 (isp t1) t2 = (comp t1 t2)
}

fun comp3 : ISP -> SP -> SP
{
comp3 t1 (isp t2) = isp (comp t1 t2) 
}

}