-- 2010-08-31

mutual {

  data L +(A : Set) : Set
  { l1 : A -> L A -> L A
  ; l2 : T A -> L A
  }

  data T +(A : Set) : Set
  { t1 : L A -> T A
  ; t2 : (A -> T A) -> T A
  }

}
