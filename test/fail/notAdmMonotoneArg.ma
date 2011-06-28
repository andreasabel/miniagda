
sized codata Stream (+ A : Set) : Size -> Set {
  cons : (i : Size) -> A -> Stream A i -> Stream A ($ i)
}

data Unit : Set {
  triv : Unit
}
 
cofun bla : (i : Size) -> (Stream Unit i -> Stream Unit i) -> Stream Unit i
{
 bla ($ i) f = f (cons Unit i triv (bla i f)) 
}
