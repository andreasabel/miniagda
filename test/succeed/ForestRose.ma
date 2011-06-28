-- 2010-09-01

data List ++(A : Set) : Set
{ nil  : List A
; cons : A -> List A -> List A
} 

mutual {

  data Rose ++ (A : Set) : Set
  { rose : (label : A) -> (subtrees : Forest A) -> Rose A
  }

  fun Forest : ++ Set -> Set
  { Forest A = List (Rose A)
  }

}

