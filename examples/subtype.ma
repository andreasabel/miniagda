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

data Ty : Set 
{
base : Ty;
arr : Ty -> Ty -> Ty
}

fun subty : Ty -> Ty -> Bool
{
subty base base = tt;
subty (arr x x') (arr y y') = and (subty y x) (subty x' y') ;
subty _ _ = ff
}
