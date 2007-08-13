data Bool : Set
{
	tt : Bool;
	ff : Bool
}

data Unit : Set
{
	unit : Unit
}

fun id : Bool -> Bool
{
id x = x 
}

fun  F : Bool -> Set
{
F tt = Unit;
F ff = Bool
}

fun f : ( x : Bool ) -> F x
{
f tt = unit;
f ff = tt
}

data Nat : Set
{
	zero : Nat;
	succ : Nat -> Nat
}

fun zz : Nat -> Nat
{
zz zero = zero;
zz (succ x) = zz x
}

fun isZero : Nat -> Bool
{
isZero zero = tt;
isZero (succ x) = ff
}
