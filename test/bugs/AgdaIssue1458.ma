data Empty { e1 ; e2 }

fun absurd : [A : Set] -> Empty -> A
{}

data R (i : Size)
{ delay (force : [j < i] -> Empty & (Empty -> R j))
} fields force

fun d : [i : Size] -> (r : R i) -> [j < i] -> Empty
{ d i r j = case (r .force j) { (x,y) -> x } }

fun ifix : ([i : Size] -> R i -> Empty) -> [i : Size] -> R i
{ ifix f i .force j = f j (ifix f j) , absurd (R j) }

-- d i (ifix (\ i r -> f i (d i r)) i) j
-- = (\ i r -> f i (d i r)) j (ifix (\ i r -> f i (d i r)) j)
-- = f j (d j (ifix (\ i r -> f i (d i r)) j))

fun fix : (f : [i : Size] -> ([j < i] -> Empty) -> Empty)
  -> [i : Size] -> [j < i] -> Empty
{ fix f i = d i (ifix (\ i r -> f i (d i r)) i) }

-- fun f : [i : Size] -> ([j < i] -> Empty) -> Empty {}
-- eval let test = fix f # #

data Eq [A : Set](a : A) : A -> Set
{ refl : Eq A a a }

fun loop : (f : [i : Size] -> ([j < i] -> Empty) -> Empty) (x : Empty) ->
  Eq Empty (fix f # #) x -- (f # (d # (fix f # #)))
{ loop f x = refl }
