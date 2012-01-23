-- 2010-05-11

data Unit : Set
{ unit : Unit 
}

sized codata Stream (+ A : Set) : Size -> Set 
{ cons : [i : Size] -> (head : A) -> (tail : Stream A i) -> Stream A ($ i)
}
fields head, tail
 
-- the type of f is not admissible
cofun f : (Stream Unit # -> Stream Unit #) ->
  (i : Size) -> (Stream Unit i -> Stream Unit #) -> Stream Unit i
{ f h ($ j) g = 
    h (g (cons j unit (f (\ x -> h (h x)) j (\ x -> g (cons j unit x))))) 
}

let bla : Stream Unit # = f (tail Unit #) # (\ x -> x)

-- LOOP!
eval let u : Unit = head Unit # bla
eval let us : Stream Unit # = tail Unit # bla
