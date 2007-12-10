data Nat : Set  
{
	zero : Nat ;
	succ : Nat -> Nat
}


fun id : Nat -> Nat
{
id x = x 
}

let one : Nat = id (succ zero)

fun add : Nat -> Nat -> Nat 
{
add x zero = x ;
add x (succ y) = succ (add x y) 
}

let three : Nat = add (succ (succ zero)) (succ (zero))

data Eq ( A : Set ) : A -> A -> Set
{
refl : (a : A) -> Eq A a a 
}

let proof : (x : Nat ) -> Eq Nat (add x zero) x = \ y -> refl Nat y  

-- does not type check
--let proof2 : ( x : Nat ) -> Eq Nat (add zero x) x = \ y -> refl Nat y

fun eqsucc : (x : Nat ) -> (y : Nat ) -> Eq Nat x y -> Eq Nat (succ x) (succ y)
{
eqsucc .x .x (refl .Nat x) = refl Nat (succ x)
}

fun proof2 : ( x : Nat ) -> Eq Nat (add zero x) x
{
proof2 zero = refl Nat zero;
proof2 (succ x) = eqsucc (add zero x) x (proof2 x)
} 


data Bool : Set
{
tt : Bool;
ff : Bool
}

mutual{

fun even : Nat -> Bool
{
  even zero = tt;
  even (succ x) = odd x ;
}

fun odd : Nat -> Bool
{
  odd zero = ff;
  odd (succ x) = even x ;
}

}


fun ack : Nat -> Nat -> Nat
{
ack zero y = y;
ack (succ x) zero = ack x (succ zero);
--ack (succ x) (succ y) = ack x (ack (succ x) y)
}


fun bla : Nat -> Nat
{
bla zero = bla (succ zero);
bla (succ x) = bla x
}

eval let bla2 : Nat = bla zero