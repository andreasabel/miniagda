-- 2010-08-30

-- this is negative

mutual {

  data D : Set { absD : (E -> D) -> D }
  data E : Set { inE  : D -> E }

}