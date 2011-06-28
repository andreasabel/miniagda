-- 2010-09-14

-- this needs to be rejected

data U : Set #
{ inn : [i : Size] -> (out : Set i) -> U
}

let U' : U = inn # U

