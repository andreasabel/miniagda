data Nat : Set
{

zero : Nat;
succ : Nat -> Nat

}

data Bool : Set 
{

tt : Bool;
ff : Bool
}

fun and : Bool -> Bool -> Bool
{
and tt b = b ;
and ff b = ff
}

data Ty : Nat -> Set 
{
base : ( n : Nat ) -> Ty (succ n);
arr : ( n : Nat ) -> Ty n -> Ty n -> Ty (succ n)
}

fun subty : ( n : Nat ) -> Ty n -> Ty n -> Bool
{
subty (succ n) (base .n) (base .n) = tt;
subty (succ n) (arr .n x y) (arr .n x' y') = and (subty n x' x) (subty n y y');
subty _ _ _ = ff
}
