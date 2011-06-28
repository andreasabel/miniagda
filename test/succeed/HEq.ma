data HEq [A : Set](a : A) : [B : Set] -> B -> Set
{ refl : HEq A a A a
}

data HEq' [i : Size][A : Set i](a : A) : [B : Set i] -> B -> Set
{ refl' : HEq' i A a A a
}
 