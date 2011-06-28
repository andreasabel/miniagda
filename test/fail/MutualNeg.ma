-- 2010-08-30

-- this is positive, but not strictly positive

mutual {

  data D : Set { absD : (E -> D) -> D }
  data E : Set { absE : (D -> E) -> E }

}