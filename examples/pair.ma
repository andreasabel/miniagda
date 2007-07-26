data Bool : Set
{
	tt : Bool;
	ff : Bool
}

data Pair : Set
{
	pp : Bool -> Bool -> Pair
}

fun left : Pair -> Bool
{

(pp x y) = x

}

fun right : Pair -> Bool
{
(pp x y) = y
}

const a : Pair = pp ff tt 

const f : Bool = left a

const t : Bool = right a  