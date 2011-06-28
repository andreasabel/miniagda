-- 2010-08-28  fake mutuals, to test positivity checker

-- real mutuals

mutual { 
  data E : Set { e0 : E
               ; eS : O -> E }
  data O : Set { oS : E -> O }
}

mutual {
  data D1 : Set { d1 : D2 -> D1 }   -- D1 /  D2 ++ D3 /
  data D2 : Set { d2 : D3 -> D2 }   -- D1 /  D2 /  D3 ++
  data D3 : Set { d3 : D1 -> D3 }   -- D1 ++ D2 /  D3 /
  {- to see that D1 is spos, we have to traverse the calls through D2 and D3 -}
}

-- fake mutuals

mutual {  

  -- A is spos in its def.
  data A : Set 
  { a1 : A
  ; a2 : A -> A
  }

  -- but not in B
  data B : Set 
  { b1  : B
  ; b2  : (A -> B) -> B
  }
}

mutual {

  -- D is spos in its definition
  data D : Set 
  { c : D
  ; d : D -> D
  }
  -- D is not spos in T
  fun T : Set -> Set
  { T X = D -> X
  }

}