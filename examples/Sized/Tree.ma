data Tree : Set
{ leaf : Tree
; node : (left : Tree) -> (right : Tree) -> Tree
}

-- this should elaborate to:

sized data TreeS : Size -> Set
{ leafS : TreeS ($ 0)  -- 0 is the empty maximum
; nodeS : [left_size  : Size] -> (left  : TreeS left_size)  ->
          [right_size : Size] -> (right : TreeS right_size) ->
          TreeS $(max left_size right_size)
}

fun id : [i : Size] -> TreeS i -> TreeS i
{ id .($ 0) leafS = leafS
; id .$(max i j) (nodeS i l j r) = nodeS i (id i l) j (id j r)
}

{- attributes

data Tree<size:Size> : Set
{ leaf : [i : Size] -> Tree<size = $ i>
; node : (left : Tree) -> (right : Tree) -> Tree<size = $(max (size left) (size right))
}

Tree : {size : Size} -> Set

Tree --> Tree {size = ?}

-}
