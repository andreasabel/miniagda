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


data ISP (+ K : Set ) : Set
{
put : Nat -> K -> ISP K ; 
get : (Nat -> ISP K) -> ISP K; 
}

codata SP : Size -> Set
{
isp : (i : Size ) -> ISP (SP i) -> SP ($ i);
}


mutual {

cofun blub : (i : Size ) -> Stream i -> Stream ($ i)
{
blub .($ i) (cons i x xs) = cons ($ i) x (bla ($ i)) 
}

cofun bla : (i : Size ) -> Stream i 
{
bla ($ i) = blub i (bla i)
}

}



mutual {

fun ieat : (i : Size ) -> ISP (SP #) -> Stream # -> Stream ($ i)
{
ieat i (get .(SP #) f) (cons .# a as) = ieat i (f a) as ;
ieat i (put .(SP #) b sp) as  = cons i b (eat i sp as)
}

cofun eat : (i : Size ) -> SP # -> Stream # -> Stream i 
{
eat ($ i) (isp .# sp) as = ieat i sp as   
}

}



mutual {

cofun adder : (i : Size ) -> SP i 
{
adder ($ i) = isp i (get (SP i) (\ n -> adder' i n zero))
}

fun adder' : (i : Size ) -> Nat -> Nat -> ISP (SP i) 
{
adder' i zero acc = put (SP i) acc (adder i);
adder' i (succ n) acc = get (SP i) (\ m -> (adder' i n (add m acc)))
}

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
const fours : Stream # = eat # (adder #) (twos #) 

eval const four : Nat = nth two fours

mutual {

cofun comp : (i : Size ) -> SP # -> SP # -> SP i
{
comp ($ i) (isp .# t1) (isp .# t2) = isp i (comp1 i t1 t2) 
}

fun comp1 : (i : Size ) -> ISP (SP #) -> ISP (SP #) -> ISP (SP i)
{
comp1 i t1 (put .(SP #) c t2) = put (SP i) c (comp i (isp # t1) t2);
comp1 i (put .(SP #) b t1) (get .(SP #) f2) = comp2 i t1 (f2 b);
comp1 i (get .(SP #) f1) t2 = get (SP i) (\a -> comp1 i (f1 a) t2) 
}

fun comp2 : (i : Size ) -> SP # -> ISP (SP #) -> ISP (SP i)
{
comp2 i (isp .# t1) t2 = comp1 i t1 t2
}

}

const compadder : SP # = comp # (adder #) (adder #)

eval const 16 : Nat = nth two (eat # compadder (twos #))

mutual {

cofun jump : (i : Size ) -> SP i 
{
jump ($ i) = isp i (get (SP i) (\ n -> jump' i n n))
}

fun jump' : (i : Size ) -> Nat -> Nat -> ISP (SP i) 
{
jump' i zero x = put (SP i) x(jump i);
jump' i (succ n) x= get (SP i) (\ m -> (jump' i n x))
}

}


eval const diverge : Nat = nth two (bla #)