
sized codata Stream (+ A : Set) : Size -> Set {
  cons : (i : Size) -> A -> Stream A i -> Stream A ($ i)
}

data Unit : Set {
  triv : Unit
}

cofun bla : (i : Size) -> ((j : Size) -> Stream Unit j -> Stream Unit j) -> Stream Unit i
{
 bla ($ i) f = f ($ i) (cons i triv (bla i f))
}
