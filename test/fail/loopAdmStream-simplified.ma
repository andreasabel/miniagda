-- 2010-05-11

sized codata StreamUnit : Size -> Set
{ cons : [i : Size] -> (tail : StreamUnit i) -> StreamUnit ($ i)
}
fields tail

-- the type of f is not admissible
cofun f : (StreamUnit # -> StreamUnit #) ->
  (i : Size) -> (StreamUnit i -> StreamUnit #) -> StreamUnit i
{ f h ($ j) g = h (g (cons j (f (\ x -> h (h x)) j (\ x -> g (cons j x)))))
}

let bla : StreamUnit # = f (tail #) # (\ x -> x)

-- LOOP!
eval let us : StreamUnit # = tail # bla
